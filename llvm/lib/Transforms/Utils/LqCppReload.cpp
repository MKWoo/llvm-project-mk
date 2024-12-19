//===-- LqCppReload.cpp - Example Transformations --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/LqCppReload.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"

#include "llvm/Transforms/Utils/city.h"
#include <fcntl.h>

#if defined(_WIN32)
#include <windows.h>
#elif defined(__APPLE__)
#include <sys/file.h>
#include <unistd.h>
#endif

#include <iostream>
#include <fstream>
#include <map>

#define DEBUG_TYPE "runtime-function-call-counter"

using namespace llvm;


#define defUSeSymDataOffset 1 //用nm的符号文件,而不是收集函数 . 为1的话，只收集函数的signature

//LqRldSaveDir="your_value"
//保存收集信息的文件夹
static llvm::cl::opt<std::string> LqCppRldOption(
	"LqRldSaveDir",
	llvm::cl::desc("This is LqRldSaveDir for my pass"),
	llvm::cl::value_desc("LqRldSaveDir value"),
	llvm::cl::init("default value")
);

//控制是否要收集信息，true：是需要执行patch的cpp，需要收集信息，并打印日志；
// false：只需要执行给函数增加patch逻辑，不需要收集信息，也不需要打日志。不让日志会特别多；
static llvm::cl::opt<bool> LqCppRldNeedSave(
	"LqRldSave",
	llvm::cl::desc("This is LqCppRldNeedSave for my pass"),
	llvm::cl::value_desc("LqCppRldNeedSave value"),
	llvm::cl::init(false)
);

namespace helper
{
	Constant* CreateGlobalVariable(Module& module, StringRef globalVariableName);

	bool PatchFunctionCallVM(Module& module);

#if defUSeSymDataOffset
	bool CollectFunctionSignature(Module& module);
#else
	bool CreateCollectAddressFunction(Module& module);
#endif
	bool Create_Init_Module_Function(Module& module);

	//bool BuildWrapperFunction(Module& module);


} // namespace helper

LqCppReloadModulePass::LqCppReloadModulePass() : OS(dbgs()) {}
LqCppReloadModulePass::LqCppReloadModulePass(raw_ostream& OS, const std::string& Banner,
	bool ShouldPreserveUseListOrder,
	bool EmitSummaryIndex)
	: OS(OS), Banner(Banner),
	ShouldPreserveUseListOrder(ShouldPreserveUseListOrder),
	EmitSummaryIndex(EmitSummaryIndex) {}

//PreservedAnalyses TestRun(Module& M, AnalysisManager<Module>&)
//{
//
//	bool changed = false;
//	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
//}


Constant* helper::CreateGlobalVariable(Module& module, StringRef globalVariableName)
{
	auto& context = module.getContext();

	// This will insert a declaration into module
	Constant* newGlobalVariable = module.getOrInsertGlobal(globalVariableName, IntegerType::getInt32Ty(context));

	// This will change the declaration into definition (and initialize to 0)
	GlobalVariable* initializedGlobalVariable = module.getNamedGlobal(globalVariableName);
	initializedGlobalVariable->setLinkage(GlobalValue::CommonLinkage);
	initializedGlobalVariable->setAlignment(MaybeAlign(4));
	initializedGlobalVariable->setInitializer(ConstantInt::get(context, APInt(32, 0)));

	return newGlobalVariable;
}


bool isWrapperFunc(Function& F) {
	// 获取函数的属性列表
	AttributeList attrList = F.getAttributes();
	// 获取名为"IsWrapperFunc"的属性
	Attribute attr = attrList.getFnAttr("IsWrapperFunc");
	// 检查属性是否存在
	if (attr.isValid()) {
		// 获取属性的值并将其转换为字符串
		StringRef value = attr.getValueAsString();
		// 判断值是否为1
		return value == "1";
	}

	// 如果属性不存在，返回false
	return false;
}


std::string getFunctionID(Function& F) {
	// 获取函数的属性列表
	AttributeList attrList = F.getAttributes();
	// 获取名为"funcID"的属性
	Attribute attr = attrList.getFnAttr("funcID");
	// 检查属性是否存在
	if (attr.isValid()) {
		// 获取属性的值并将其转换为字符串
		StringRef value = attr.getValueAsString();
		// 返回属性值
		return value.str();
	}

	// 如果属性不存在，返回空字符串
	return "";
}


//PreservedAnalyses LqCppReloadModulePass::TestRun(Module& M, ModuleAnalysisManager& AM) {
//
//	errs() << "*******Enter LqCppReloadModulePass::run*******" << M.getName() << "\n";
//
//	errs() << "minkee func: " << __FUNCTION__ << M.getName()<< "\n";
//
//// 	bool changed = helper::CountFunctionCallsInModule(M);
//// 	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
//
//	return PreservedAnalyses::all();
//}
// 
// 
// 
// Function to extract the second part of the string
std::string extractSecondPart(const std::string& s) {
	size_t firstDot = s.find('.');
	if (firstDot == std::string::npos) {
		// No dot found, entire string is first part
		return "";
	}

	size_t secondDot = s.find('.', firstDot + 1);
	if (secondDot == std::string::npos) {
		// Only one dot in string, the second part is after the first dot
		return s.substr(firstDot + 1);
	}

	// In the case of at least two dots, extract the substring between the first and second dots
	return s.substr(firstDot + 1, secondDot - firstDot - 1);
}



uint64_t g_ModueHashForPatch = 0;
uint32_t  g_needPatchFuncCount =0;
uint32_t  g_AddressCollectedCount = 0; //收集的函数/全局变量地址  总个数，这个个数会写到结构体，注册module。 引擎new g_AddressCollectedCount个对象的数组，然后回调回来，进行收集

#define _str_LQCallVm "LQCallVm"

#define __str_g_needpatch_lqcppReload "g_needpatch_lqcppReload_"

#if !defUSeSymDataOffset
#define __str_CollectAddressFunction "CollectAddressFunction_"
#endif
//#define __wrapper_GV_stuct_construct_function_prefix "__ConstuctSt_"

PreservedAnalyses LqCppReloadModulePass::run(Module& M, ModuleAnalysisManager& AM) {
	std::string Hash_str =  M.getSourceFileName();

	// 使用 CityHash64 计算 64 位哈希值
	g_ModueHashForPatch = CityHash64(Hash_str.c_str(), Hash_str.size());

	//日志例子：####### Start_IR_handle   #######  M:Runtime/Engine/Private/Slate/SceneViewport.cpp  moduleHash:11057503442973761409 LqCppRldOption:/Volumes/mkSAMSUNG-4/tools/LqRldCollect
	errs() << "####### Start_IR_handle " /*<< __FUNCTION__*/ << "  #######  M:" << Hash_str <<"  moduleHash:" << g_ModueHashForPatch << " LqCppRldOption:"<<LqCppRldOption <<"\n";

	bool changed = true;
	//changed &= helper::BuildWrapperFunction(M);
	changed &= helper::PatchFunctionCallVM(M);

#if defUSeSymDataOffset
	if (LqCppRldNeedSave)
	{
		changed &= helper::CollectFunctionSignature(M);
	}
#else
	changed &= helper::CreateCollectAddressFunction(M);
#endif

	changed &= helper::Create_Init_Module_Function(M);

	errs() << "\n" << "####### End_IR_handle " /*<< __FUNCTION__*/ << "  #######  M:" << M.getName() << "\n\n\n";
	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}

//和 G:\llvm-project\llvm\include\llvm\IR\Type.h 的enum TypeID对应
const char*  TypeID_str[] {
	// PrimitiveTypes
	"HalfTyID = 0",  ///< 16-bit floating point type
	"BFloatTyID",    ///< 16-bit floating point type (7-bit significand)
	"FloatTyID",     ///< 32-bit floating point type
	"DoubleTyID",    ///< 64-bit floating point type
	"X86_FP80TyID",  ///< 80-bit floating point type (X87)
	"FP128TyID",     ///< 128-bit floating point type (112-bit significand)
	"PPC_FP128TyID", ///< 128-bit floating point type (two 64-bits", PowerPC)
	"VoidTyID",      ///< type with no size
	"LabelTyID",     ///< Labels
	"MetadataTyID",  ///< Metadata
	"X86_MMXTyID",   ///< MMX vectors (64 bits", X86 specific)
	"X86_AMXTyID",   ///< AMX vectors (8192 bits", X86 specific)
	"TokenTyID",     ///< Tokens
	"IntegerTyID",        ///< Arbitrary bit width integers
	"FunctionTyID",       ///< Functions
	"PointerTyID",        ///< Pointers
	"StructTyID",         ///< Structures
	"ArrayTyID",          ///< Arrays
	"FixedVectorTyID",    ///< Fixed width SIMD vector type
	"ScalableVectorTyID", ///< Scalable SIMD vector type
	"TypedPointerTyID",   ///< Typed pointer used by some GPU targets
	"TargetExtTyID",      ///< Target extension type
};

const char* TypeID_str_oneAlpha[]{
	"A", // "HalfTyID = 0",  ///< 16-bit floating point type
	"B", // "BFloatTyID",    ///< 16-bit floating point type (7-bit significand)
	"C", // "FloatTyID",     ///< 32-bit floating point type
	"D", // "DoubleTyID",    ///< 64-bit floating point type
	"E", // "X86_FP80TyID",  ///< 80-bit floating point type (X87)
	"F", // "FP128TyID",     ///< 128-bit floating point type (112-bit significand)
	"G", // "PPC_FP128TyID", ///< 128-bit floating point type (two 64-bits, PowerPC)
	"H", // "VoidTyID",      ///< type with no size
	"I", // "LabelTyID",     ///< Labels
	"J", // "MetadataTyID",  ///< Metadata
	"K", // "X86_MMXTyID",   ///< MMX vectors (64 bits, X86 specific)
	"L", // "X86_AMXTyID",   ///< AMX vectors (8192 bits, X86 specific)
	"M", // "TokenTyID",     ///< Tokens
	"N", // "IntegerTyID",        ///< Arbitrary bit width integers
	"O", // "FunctionTyID",       ///< Functions
	"P", // "PointerTyID",        ///< Pointers
	"Q", // "StructTyID",         ///< Structures
	"R", // "ArrayTyID",          ///< Arrays
	"S", // "FixedVectorTyID",    ///< Fixed width SIMD vector type
	"T", // "ScalableVectorTyID", ///< Scalable SIMD vector type
	"U", // "TypedPointerTyID",   ///< Typed pointer used by some GPU targets
	"V"  // "TargetExtTyID",      ///< Target extension type
};






std::string getTypeNameOneAlpha(Type* Ty) {
	if (Ty->getTypeID() > llvm::Type::TypeID::TargetExtTyID)
	{
		int* p = 0; *p = 0; //force crash
	}

	std::string strType = TypeID_str_oneAlpha[Ty->getTypeID()];
	return strType;
}

//PQS 返回类型+参数类型 ，没有空格，没有下划线。 用于序列化函数类型
std::string getFunctionTypeShortDesc(Function& F) {
	std::string Signature = getTypeNameOneAlpha(F.getReturnType());
	for (auto& Arg : F.args()) {
		Signature +=  getTypeNameOneAlpha(Arg.getType());
	}

	//errs() << "      getFunctionTypeShortDesc  " << "func: "<<F.getName()<<"  Signature:" << Signature << "\n";

	return Signature;
}

std::string getFunctionTypeShortDescIndirect(FunctionType* funcType) {
	std::string Signature = getTypeNameOneAlpha(funcType->getReturnType());
	for (unsigned i = 0; i < funcType->getNumParams(); ++i) {
		Signature += getTypeNameOneAlpha(funcType->getParamType(i));
	}

	//errs() << "      getFunctionTypeShortDesc  " << "func: "<<F.getName()<<"  Signature:" << Signature << "\n";

	return Signature;
}


/////////////////////////////////////////////////////WrapperFun相关 --废弃

//std::string getTypeName(Type* Ty) {
//	if (Ty->getTypeID() > llvm::Type::TypeID::TargetExtTyID)
//	{
//		int* p = 0; *p = 0; //force crash
//	}
//
//	std::string strType = TypeID_str[Ty->getTypeID()];
//	return strType;
//}

//#define WrapperFuncStartStr "wrapper_"
//
//std::string getFunctionSignature(Function* F) {
//	std::string Signature = WrapperFuncStartStr/*"wrapper_"*/ + getTypeName(F->getReturnType());
//	for (auto& Arg : F->args()) {
//		Signature += "_" + getTypeName(Arg.getType());
//	}
//	return Signature;
//}

//uint64 FromFuncGetWrapperFuncSignatureHash(Function& F)
//{
//	std::string funcSignature = getFunctionSignature(&F);
//	uint64 WrapperNameHash = CityHash64(funcSignature.c_str(), funcSignature.size());
//	return WrapperNameHash;
//}	


////void * func_UE(void * runtime, void * _ctx, uint64_t * _sp, void * _mem, void* funAddress)
//FunctionType* getWrapperFunctionType(LLVMContext& Context) {
//	return FunctionType::get(Type::getInt8PtrTy(Context), {
//		Type::getInt8PtrTy(Context), // runtime
//		Type::getInt8PtrTy(Context), // _ctx
//		Type::getInt64PtrTy(Context), // _sp
//		Type::getInt8PtrTy(Context),  // _mem
//		Type::getInt8PtrTy(Context)   // funAddress
//		}, false);
//}
/////////////////////////////////////////////////////WrapperFun相关 --废弃


// 给每个函数分配一个数字ID //整个module里面的函数id是基于g_funcID递增的
int g_funcID = 0;

bool IsWrapperFunction(Function& F){return false;}

// Helper function to convert a character to lowercase
char toLowerChar(char ch) {
	return std::tolower(static_cast<unsigned char>(ch));
}
bool startsWithIgnoreCase(const std::string& str, const std::string& prefix) {
	if (str.size() < prefix.size()) {
		return false;
	}

	for (std::size_t i = 0; i < prefix.size(); ++i) {
		if (toLowerChar(str[i]) != toLowerChar(prefix[i])) {
			return false;
		}
	}
	return true;
}

bool IsSkipPatchByFunctioName(const std::string &origFuncName)
{
	/*(origFuncName.npos != origFuncName.find("printf")) || (origFuncName.npos != origFuncName.find("main"))*/

	if(origFuncName == _str_LQCallVm || startsWithIgnoreCase(origFuncName, "__cxx_global_var_init") || startsWithIgnoreCase(origFuncName, "__cxa_atexit"))
		return true;

	return false;
}

bool helper::PatchFunctionCallVM(Module& M)
{
	errs() << "\n" << "**Enter " << "start_patch" << " M:" << M.getName() << " moduleHash:" << g_ModueHashForPatch <<"\n";

	LLVMContext& context = M.getContext();


	// Step 1: Count non-declaration functions
	int patchFuncCount = 0;
	for (Function& F : M) {
		if (!F.isDeclaration() && !F.empty() && !F.isIntrinsic() && !IsWrapperFunction(F)) {
			++patchFuncCount;
		}
	}

	g_needPatchFuncCount = patchFuncCount; //g_needPatchFuncCount会再ini 函数，进行注册时用到

	 // Step 2: Create a global boolean array:   char g_needPatch[g_needPatchFuncCount] = {0};  //ConstantAggregateZero init to 0
	Type* Bytetype = Type::getInt8Ty(M.getContext());
	ArrayType* arrayType = ArrayType::get(Bytetype, patchFuncCount);

	GlobalVariable* g_needPatch_bool_array = new GlobalVariable(
		M, arrayType, false, GlobalValue::InternalLinkage, ConstantAggregateZero::get(arrayType),
		__str_g_needpatch_lqcppReload + std::to_string(g_ModueHashForPatch));

	// 创建CallVm函数原型 bool CallVm( /*char* strFunName,*/ void* pParameters, int paraCount, int funcID)    pParameters是栈上的原函数参数组成的数据
	FunctionType* callVmType = FunctionType::get(Type::getInt64Ty(context), { /*Type::getInt8PtrTy(context),*/ Type::getInt64PtrTy(context), Type::getInt32Ty(context), Type::getInt32Ty(context) }, false);
	Function* callVmFunc = Function::Create(callVmType, GlobalValue::ExternalLinkage, _str_LQCallVm, M);

	for (Function& F : M) {

		if (F.empty() || F.isDeclaration() || F.isIntrinsic() /* || IsWrapperFunction(F)*/)
		{
			if (LqCppRldNeedSave)
			{
				errs() << "skip_func:" << F.getName() << "\n";
			}

			continue;
		}

		F.addFnAttr("funcID", std::to_string(g_funcID));

		std::string origFuncName = F.getName().str();
		if (IsSkipPatchByFunctioName(origFuncName))
		{
			if (LqCppRldNeedSave)
			{
				errs() << "skip by name func:" << origFuncName << "\n";
			}

			continue;
		}

		if (LqCppRldNeedSave)
		{
			errs() << "patch_function id:" << g_funcID << "  Name:" << origFuncName << "  func return type:"<< F.getReturnType()->getTypeID()<< "\n";
		}

		Module* M = F.getParent();
		LLVMContext& Ctx = M->getContext();
		IRBuilder<> builder(Ctx);

		// 获取当前函数的参数
		std::vector<Value*> args;
		for (auto& arg : F.args()) {
			args.push_back(&arg);
		}

		// 创建结构体类型 Paras是把当前函数的参数保存到一个栈结构体中, 不同函数可能不一样
		StructType* structTy = StructType::create(F.getContext(), "Paras");
		std::vector<Type*> structFields;
		for (auto arg : args) {
			structFields.push_back(arg->getType());
		}
		structTy->setBody(structFields);

		// 在每个函数的入口基本块的开始处插入一个新的基本块
		BasicBlock& entryBB = F.getEntryBlock();
		BasicBlock* patchBB = BasicBlock::Create(Ctx, "patchBB", &F, &entryBB);

		// 在patchBB中插入patch逻辑
		builder.SetInsertPoint(patchBB);
		//Value* g_test_val = builder.CreateLoad(IntegerType::getInt32Ty(context), g_test);

	// 获取g_needPatch_bool_array[funcID]的值
		Value* funcIDValue = builder.getInt32(g_funcID);
		Value* needPatchPtr = builder.CreateGEP(IntegerType::getInt8Ty(context), g_needPatch_bool_array, funcIDValue);
		Value* needPatchValue = builder.CreateLoad(IntegerType::getInt8Ty(context), needPatchPtr);

		Value* cond = builder.CreateICmpNE(needPatchValue, ConstantInt::get(IntegerType::getInt8Ty(Ctx), 0)); //数值是0  1是测试数据
		//Value* cond = builder.CreateICmpNE(needPatchValue, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 0x12345678)); //数值是0  1是测试数据
		BasicBlock* returnBB = BasicBlock::Create(Ctx, "PatchBody", &F);
		builder.CreateCondBr(cond, returnBB, &entryBB);

//PatchBody
		// 在PatchBody中插入调用call的指令和返回指令
		builder.SetInsertPoint(returnBB);
		//builder.CreateCall(callVmFunc);

		//patch 逻辑
		// 创建参数结构体实例并初始化
		Value* paras = builder.CreateAlloca(structTy);
		for (size_t i = 0; i < args.size(); ++i) {
			Value* field = builder.CreateStructGEP(structTy, paras, i);
			builder.CreateStore(args[i], field);
		}

		// 调用CallVm函数
		//Value* funcName = builder.CreateGlobalStringPtr(F.getName()); //// 获取当前函数名
		Value* ArgCount = builder.getInt32(args.size());
		//Value* funcIDValue = builder.getInt32(funcID);
		Value* callVmArgs[] = { /*funcName,*/ paras, ArgCount, funcIDValue };
		Value* VmReturnValue = builder.CreateCall(callVmType, callVmFunc, callVmArgs); //vm函数返回值是uint64类型

		// Get the current function's return type
		Type* CurFReturnType = F.getReturnType();

		//处理返回值
		if (F.getReturnType()->isVoidTy()) {
			builder.CreateRetVoid();
		}
		else {
			//builder.CreateRet(VmReturnValue/*Constant::getNullValue(F.getReturnType())*/);
			Value* ConvertedReturnValue = VmReturnValue;

			// If the types differ, perform a cast
			if (CurFReturnType != VmReturnValue->getType()) {
				if (CurFReturnType->isIntegerTy() /*&& VmReturnValue->getType()->isIntegerTy()*/) {
					// Integer to integer cast
					ConvertedReturnValue = builder.CreateIntCast(VmReturnValue, CurFReturnType, false);
				}
				else if (CurFReturnType->isFloatingPointTy() /*&& VmReturnValue->getType()->isFloatingPointTy()*/) {
					//ConvertedReturnValue = builder.CreateSIToFP(VmReturnValue, CurFReturnType, "toFloat"); 可以转，但是数据不对：cvtsi2sd xmm6,rax rax:0x40132d0e56041894 4.794 。最后日志打印的double数据不对
					Value* ConvertedDoubleValue = builder.CreateBitCast(VmReturnValue, Type::getDoubleTy(context), "bitcastToDouble");

					//先把VmReturnValue返回值转为double，因为VmReturnValue返回uint64，和double都是64bit宽度的。可以转。
					//然后再把double转成对应的float等
					switch (CurFReturnType->getTypeID()) {
					case Type::TypeID::DoubleTyID:
						ConvertedReturnValue = ConvertedDoubleValue;
						break;
					case Type::TypeID::FloatTyID:
					{
						Value* FloatValue = builder.CreateFPTrunc(ConvertedDoubleValue, Type::getFloatTy(context), "doubleToFloat");
						ConvertedReturnValue = FloatValue;
					}
						break;
					case Type::TypeID::HalfTyID:
					{
						Value* HalfValue = builder.CreateFPTrunc(ConvertedDoubleValue, Type::getHalfTy(context), "doubleToHalf");
						ConvertedReturnValue = HalfValue;
					}
						break;
					case Type::TypeID::BFloatTyID:
					{
						Value* BFFloatValue = builder.CreateFPTrunc(ConvertedDoubleValue, Type::getBFloatTy(context), "doubleToBFloat");
						ConvertedReturnValue = BFFloatValue;
					}
						break;
					case Type::TypeID::FP128TyID:
					default:
						ConvertedReturnValue = Constant::getNullValue(F.getReturnType());
						break;
					}
				}
				else if (CurFReturnType->isPointerTy() /*&& VmReturnValue->getType()->isPointerTy()*/) {
					// Pointer to pointer cast
					ConvertedReturnValue = builder.CreateBitOrPointerCast(VmReturnValue, CurFReturnType);
				}
				else {
					errs() << "Unsupported return type conversion. funcReturnType:"<< CurFReturnType->getTypeID() <<"\n";
					ConvertedReturnValue = Constant::getNullValue(F.getReturnType());
				}
			}

			// Return the possibly converted value
			builder.CreateRet(ConvertedReturnValue);
		}

		//patch 逻辑
//PatchBody

		++g_funcID;
	}

	errs() << "**Leave " << "start_patch" << " M:" << M.getName() << "\n";

	return true;
}




/// <收集信息定义>
//collect address
enum emCollectType
{
	em_type_function = 0,
	em_type_globalValue = 1,
};


struct CollectItemInfo
{
	//uint32_t collectIndex = 0; //在数组的存放index，调用Builder.CreateStore时index +1。 当前collectIndex的最大值决定回调CollectAddressFunction时，需要new的数组大小
	uint32_t collectType = 0; //函数或全局变量 emCollectType
	std::string funcSignature; //PQS 返回值类型+参数类型//wrapper_VoidTyID_PointerTyID_PointerTyID_PointerTyID_PointerTyID_PointerTyID_PointerTyID 字符串函数，wrapper_+函数返回值类型+参数类型

	CollectItemInfo() :/*collectIndex(0),*/ collectType(0)
	{}
	CollectItemInfo(const CollectItemInfo& other) :/*collectIndex(other.collectIndex), */collectType(other.collectType), funcSignature(other.funcSignature)
	{}
	CollectItemInfo(/*uint32_t collectIndex_in,*/ uint32_t collectType_in, std::string funcSignature_in) :/*collectIndex(collectIndex_in), */collectType(collectType_in), funcSignature(funcSignature_in)
	{}
};

struct stSavedCollectInfo //序列化数据到本地
{
	uint64_t moduleHash;
	std::map<std::string, CollectItemInfo> mapCollectAddressData; //Name:?MinkeeTestFunc@@YA_NPEB_W@Z        patch_function id:109  //记录index 和名字关系，

	stSavedCollectInfo() :moduleHash(0) {}
};

stSavedCollectInfo g_collectInfo; //序列化数据到本地
/// <收集信息定义>


// 文件锁定
std::mutex fileMutex;

// 保存到二进制文件（追加）
void saveCollectDataToBinaryFile(const std::string& filename) {

	if (!LqCppRldNeedSave)
	{
		errs() << "\n" << "** " << "save_collectData" << " **No need to Save:" << filename << " hash:" << g_collectInfo.moduleHash << "\n";
		return ;
	}

	std::lock_guard<std::mutex> lock(fileMutex);

#if defined(__APPLE__)
	int fd = open(filename.c_str(), O_WRONLY | O_CREAT /*| O_APPEND*/, 0666);
	if (fd == -1) {
		std::cerr << "Failed to open the file." << std::endl;
		return;
	}
#endif


#if defined(_WIN32)
	// 若不存在名为"pmutex"的互斥量则创建它；否则获取其句柄
	HANDLE hMutex = CreateMutexA(NULL, false, "pmutex_save_collctInfo_75FB33D1-DC3C-4976-81FC-F4D6A5B38036");
	if (NULL == hMutex)
	{
		return ;
	}
#elif defined(__APPLE__)
	// 锁定文件
	if (flock(fd, LOCK_EX) == -1) {
		std::cerr << "Failed to lock the file." << std::endl;
		close(fd);
		return;
	}
#endif

	std::ofstream outFile(filename, std::ios::binary/* | std::ios::app*/);
	outFile.write(reinterpret_cast<const char*>(&g_collectInfo.moduleHash), sizeof(g_collectInfo.moduleHash));

	size_t mapSize = g_collectInfo.mapCollectAddressData.size();
	outFile.write(reinterpret_cast<const char*>(&mapSize), sizeof(mapSize));

	for (const auto& [key, value] : g_collectInfo.mapCollectAddressData) {
		size_t keySize = key.size();
		outFile.write(reinterpret_cast<const char*>(&keySize), sizeof(keySize));
		outFile.write(key.c_str(), keySize);
		//outFile.write(reinterpret_cast<const char*>(&value.collectIndex), sizeof(value.collectIndex));
		outFile.write(reinterpret_cast<const char*>(&value.collectType), sizeof(value.collectType));
		//outFile.write(reinterpret_cast<const char*>(&value.funcWrapperSignatureHash), sizeof(value.funcWrapperSignatureHash));
		size_t funcWrapperSignatureSize = value.funcSignature.size();
		outFile.write(reinterpret_cast<const char*>(&funcWrapperSignatureSize), sizeof(funcWrapperSignatureSize));
		outFile.write(value.funcSignature.c_str(), funcWrapperSignatureSize);
	}

	errs() << "\n" << "** " << "save_collectData" << " Saved " << filename << " hash:" << g_collectInfo.moduleHash << " mapsize:" << mapSize << "\n";

#if defined(_WIN32)
	CloseHandle(hMutex); // 释放互斥量
	hMutex = NULL;
#elif defined(__APPLE__)
	flock(fd, LOCK_UN);	// 解锁文件
	close(fd);
#endif

}


bool IsFuncNameCollected(const llvm::StringRef& funcName)
{
	if (g_collectInfo.mapCollectAddressData.find(funcName.data()) != g_collectInfo.mapCollectAddressData.end())
	{
		return true;
	}

	return false;
}

//LLVM intrinsic函数的概念，它们是编译器内建的特殊函数，具有已知的名称和语义。LLVM intrinsic命名以'llvm'开头，不允许定义其函数体。它们可以被重载，例如llvm.ctpop函数可以对任意宽度整数进行操作。

//只收集函数的signature
bool  helper::CollectFunctionSignature(Module& M)
{
	if (!LqCppRldNeedSave)
	{
		errs() << "\n" << "** No_need_collect. Return " << __FUNCTION__ << " M:" << M.getName() << "\n";
		return false;
	}

	errs() << "\n" << "**Enter " << __FUNCTION__ << " M:" << M.getName() << "\n";

	LLVMContext& Context = M.getContext();
	IRBuilder<> Builder(Context);

	g_collectInfo.moduleHash = g_ModueHashForPatch;


	for (Function& F : M) {

		////isIntrinsic() LLVM 提供的特殊函数，代表一些底层硬件操作或内置功能,这些函数直接映射到目标架构的特定指令，而不需要通过常规的函数调用方式完成。
		if (F.empty() || F.isDeclaration() || F.isIntrinsic() || IsWrapperFunction(F))
		{
			errs() << "skip_func:" << F.getName() << "\n";

			continue; //只收集当前函数和当前函数调用的函数。 导入的函数没有函数体，不用枚举
		}

// 		if (LqCppRldNeedSave)
// 		{
// 			errs() << "  iterate_func:" << F.getName() << "\n";
// 		}

		if (!IsFuncNameCollected(F.getName()))
		{
			CollectItemInfo oItemInfo( em_type_function, getFunctionTypeShortDesc(F));
			g_collectInfo.mapCollectAddressData[F.getName().data()] = oItemInfo;
 			errs() <<"Collect_local_func:" << F.getName() << "\n";

		}

		// Collect called functions
		for (BasicBlock& BB : F) {
			for (Instruction& Inst : BB) {
				if (auto* Call = dyn_cast<CallBase>(&Inst)) {
					if (Function* Callee = Call->getCalledFunction()) {
						if (true/*!Callee->isIntrinsic() 函数也需要*//*!Callee->isDeclaration() && Callee->hasInternalLinkage()*/) {
							if (_str_LQCallVm != Callee->getName() && !IsFuncNameCollected(Callee->getName())) //do not need collect LQCallVm. and 函数必须还未收集
							{
								CollectItemInfo oItemInfo( em_type_function, getFunctionTypeShortDesc(*Callee));
								g_collectInfo.mapCollectAddressData[Callee->getName().data()] = oItemInfo; //isIntrinsic 为true的话，getName 以llvm.开头

								if (Callee->isIntrinsic())
								{
									std::string strSecondPart =  extractSecondPart(Callee->getName().data()); //llvm.memcpy.p0.p0.i64 -> memcpy
									CollectItemInfo oItemInfo(em_type_function, getFunctionTypeShortDesc(*Callee));
									g_collectInfo.mapCollectAddressData[strSecondPart] = oItemInfo; //isIntrinsic 为true的话，getName 以llvm.开头
									errs() << "    Collect_Call_func. isIntrinsic:" << Callee->isIntrinsic() << " Name:" << Callee->getName() << " "<<" Collect ExtraName:"<< strSecondPart<< "\n";
								}
								else
								{
									errs() << "    Collect_Call_func."<< " Name:" << Callee->getName() << "\n";
								}
								
							}
						}
					}
 					else //调用虚函数
 					{
 						// We are looking for CallInst which might be indirect
 						// This is an indirect call since called function is null
 						//Type* calledType = Call->getCalledOperand()->getType();
 						//if (PointerType* pt = dyn_cast<PointerType>(calledType)) {
 						//	if (FunctionType* funcType = cast<FunctionType>(pt->getElementType())) {
 						//		std::string signature = getFunctionTypeShortDescIndirect(funcType);
 						//		std::cout << "    Virtual Function Signature. isIndirectCall:" << Call->isIndirectCall() << " signature:" << signature << "\n";
 						//	}
 						//}

						if (FunctionType* funcType = cast<FunctionType>(Call->getFunctionType())) {
							std::string signature = getFunctionTypeShortDescIndirect(funcType);
							std::cout << "    Check call function signature. isIndirectCall:" << Call->isIndirectCall() << " signature:" << signature << "\n";
						}
						else
						{
							std::cout << "    Check call function signature. isIndirectCall:" << Call->isIndirectCall() << " getFunctionType Error!!" << "\n";
 						}
					}
				}

				for (Use& U : Inst.operands()) {
					if (GlobalVariable* GV = dyn_cast<GlobalVariable>(U.get())) {

						if (!IsFuncNameCollected(GV->getName()))
						{
							CollectItemInfo oItemInfo(em_type_globalValue, "");
							g_collectInfo.mapCollectAddressData[GV->getName().data()] = oItemInfo;
							errs() << "    Collect_Used_GV:" << GV->getName() << "\n";
						}
					}
				}
			}
		}
	}

	//g_AddressCollectedCount = CollectIndex;
	std::string strSavedDir =  LqCppRldOption;

	if (!strSavedDir.empty() && strSavedDir.back() == '"') {
		strSavedDir.pop_back();  // Remove the last character
	}

	// 检查路径是否以斜杠结尾，如果没有则添加
	if (!strSavedDir.empty() && strSavedDir.back() != '/' && strSavedDir.back() != '\\') {
		strSavedDir += '/';
	}

	std::string CurFileName = M.getName().data();
		// 使用find_last_of找到最后一个路径分隔符的位置
		size_t pos = CurFileName.find_last_of("/\\");
		if (pos != std::string::npos) {
			// 提取从最后一个分隔符后的子字符串作为文件名
			CurFileName = CurFileName.substr(pos + 1);
		}

	std::string strSaveFileName = strSavedDir +"CollectData_" + CurFileName + "_"  + std::to_string(g_ModueHashForPatch) + ".dat";

	if (llvm::sys::fs::exists(strSaveFileName)) {
		llvm::sys::fs::remove(strSaveFileName.c_str());
			//std::cout << "File deleted successfully: " << strSaveFileName << std::endl;
	}
	else {
		//std::cout << "File does not exist: " << strSaveFileName << std::endl;
	}

	saveCollectDataToBinaryFile(strSaveFileName);

	errs() << "**Leave " << __FUNCTION__ << " M:" << M.getName() << " CollectData:"<< strSaveFileName <<"\n";

	return true;
}

#if !defUSeSymDataOffset
//生成函数 void CollectAddressFunction_moduleHash(void** pAddrTable) ;这个函数外部调用，获取收集的函数和变量地址
bool helper::CreateCollectAddressFunction(Module& M)
{
	errs() << "\n" << "**Enter " << __FUNCTION__ << " M:" << M.getName() << "\n";

	LLVMContext& Context = M.getContext();
	IRBuilder<> Builder(Context);

	g_collectInfo.moduleHash = g_ModueHashForPatch;

	// Define the function type for CollectAddressFunction: void(void* [])
	Type* VoidTy = Type::getVoidTy(Context);
	Type* VoidPtrTy = Type::getInt8PtrTy(Context);
	Type* VoidPtrArrTy = ArrayType::get(VoidPtrTy, 0);
	FunctionType* FuncType = FunctionType::get(VoidTy, { VoidPtrTy->getPointerTo() }, false);

	// Create the CollectAddressFunction
	Function* CollectAddrFunc = Function::Create(FuncType, Function::InternalLinkage, __str_CollectAddressFunction + std::to_string(g_ModueHashForPatch), M);
	BasicBlock* BB = BasicBlock::Create(Context, "EntryBlock", CollectAddrFunc);
	Builder.SetInsertPoint(BB);

	auto ArgIt = CollectAddrFunc->arg_begin();
	Value* ArrayArg = &(*ArgIt);

	size_t CollectIndex = 0;

	for (Function& F : M) {

		////isIntrinsic() LLVM 提供的特殊函数，代表一些底层硬件操作或内置功能,这些函数直接映射到目标架构的特定指令，而不需要通过常规的函数调用方式完成。
		if (F.empty() || F.isDeclaration() || F.isIntrinsic() || (F.getName() == __str_CollectAddressFunction + std::to_string(g_ModueHashForPatch)) || IsWrapperFunction(F))
		{
			errs() << "skip_func:" << F.getName() << "\n";
			continue; //只收集当前函数和当前函数调用的函数。 导入的函数没有函数体，不用枚举
		}

		errs() << "  iterate_func:" << F.getName() << "\n";

		if (!IsFuncNameCollected(F.getName()))
		{
			// Insert the address of the current function at the current index
			Value* FuncAddr = Builder.CreatePointerCast(&F, VoidPtrTy);
			Value* ElemPtr = Builder.CreateGEP(VoidPtrArrTy, ArrayArg, { Builder.getInt32(0), Builder.getInt32(CollectIndex++) });
			Builder.CreateStore(FuncAddr, ElemPtr);

			CollectItemInfo oItemInfo(CollectIndex-1, em_type_function, getFunctionTypeShortDesc(F));
			g_collectInfo.mapCollectAddressData[F.getName().data()] = oItemInfo;

			errs() << "index: " << CollectIndex - 1 << "    Collect_local_func:" << F.getName() << "\n";
		}

		// Collect called functions
		for (BasicBlock& BB : F) {
			for (Instruction& Inst : BB) {
				if (auto* Call = dyn_cast<CallBase>(&Inst)) {
					if (Function* Callee = Call->getCalledFunction()) {
						if (!Callee->isIntrinsic()/*!Callee->isDeclaration() && Callee->hasInternalLinkage()*/) {

							if (_str_LQCallVm != Callee->getName() && !IsFuncNameCollected(Callee->getName())) //do not need collect LQCallVm. and 函数必须还未收集
							{
								Value* CalleeAddr = Builder.CreatePointerCast(Callee, VoidPtrTy);
								Value* CalleeElemPtr = Builder.CreateGEP(VoidPtrArrTy, ArrayArg, { Builder.getInt32(0), Builder.getInt32(CollectIndex++) });
								Builder.CreateStore(CalleeAddr, CalleeElemPtr);

								CollectItemInfo oItemInfo(CollectIndex - 1, em_type_function, getFunctionTypeShortDesc(*Callee));
								g_collectInfo.mapCollectAddressData[Callee->getName().data()] = oItemInfo;

								errs() <<"index: "<< CollectIndex - 1<< "    Collect_Call_func:" << Callee->getName() << "\n";
							}

						}
					}
				}

				for (Use& U : Inst.operands()) {
					if (GlobalVariable* GV = dyn_cast<GlobalVariable>(U.get())) {

						if (!IsFuncNameCollected(GV->getName()))
						{
							Value* GVAddr = Builder.CreatePointerCast(GV, VoidPtrTy);
							Value* GVElemPtr = Builder.CreateGEP(VoidPtrArrTy, ArrayArg, { Builder.getInt32(0), Builder.getInt32(CollectIndex++) });
							Builder.CreateStore(GVAddr, GVElemPtr);

							CollectItemInfo oItemInfo(CollectIndex - 1, em_type_globalValue, "");
							g_collectInfo.mapCollectAddressData[GV->getName().data()] = oItemInfo;

							errs() << "index: " << CollectIndex - 1 << "    Collect_Used_GV:" << GV->getName() << "\n";
						}

					}
				}
			}
		}
	}

	g_AddressCollectedCount = CollectIndex;

	std::string strSaveFileName = strSavedDir + "CollectData_" + CurFileName + "_" + std::to_string(g_ModueHashForPatch) + ".dat";

	if (llvm::sys::fs::exists(strSaveFileName)) {
		llvm::sys::fs::remove(strSaveFileName.c_str());
		std::cout << "File deleted successfully: " << strSaveFileName << std::endl;
	}
	else {
		std::cout << "File does not exist: " << strSaveFileName << std::endl;
	}

	saveCollectDataToBinaryFile(strSaveFileName);

	//saveCollectDataToBinaryFile("D:\\dev-game\\IOS-patch\\TestRunVm\\testCollectAddrData.dat");

	Builder.CreateRetVoid();

	errs() <<"**Leave " << __FUNCTION__ << " M:" << M.getName() << "\n";
	return true;
}
#endif


//生成函数 void Create_Init_Module_Function //mod_init_func 类型函数
bool helper::Create_Init_Module_Function(Module& M)
{
	errs() << "\n" << "**Enter " << __FUNCTION__ << " M:" << M.getName() << "\n";

	LLVMContext& Context = M.getContext();
	IRBuilder<> Builder(Context);

	// Assume function and struct prototypes are already declared elsewhere
	StructType* ModuleInfoTy = StructType::create(Context, "module_info");
	ModuleInfoTy->setBody({
		Type::getInt64Ty(Context),  // moduleHash     
		Type::getInt8PtrTy(Context), // g_needPathValueAddress
		Type::getInt32Ty(Context), // PatchedFunctionCount
#if !defUSeSymDataOffset
		Type::getInt8PtrTy(Context), // collect_FuncVar_Info //收集函数地址
		Type::getInt32Ty(Context)/*,*/ // collectCounts //收集的总数量
#endif
		//Type::getInt32Ty(Context), // collectFuncCounts
		//Type::getInt32Ty(Context)  // collectGvarCounts
		});

	FunctionType* RegisterFuncTy = FunctionType::get(
		Type::getInt1Ty(Context), { ModuleInfoTy->getPointerTo() }, false);

	Function* RegisterFunc = dyn_cast<Function>(
		M.getOrInsertFunction("register_module_LQCppHotReload", RegisterFuncTy).getCallee());


	std::string Int_module_func_Name = "init_module_" + std::to_string(g_ModueHashForPatch);
	// Find or create the initializer function
	FunctionType* InitFuncTy = FunctionType::get(Type::getVoidTy(Context), false);
	Function* InitFunc = Function::Create(InitFuncTy, Function::InternalLinkage, Int_module_func_Name/*"init_module_fileHash"*/, M);
	BasicBlock* EntryBB = BasicBlock::Create(Context, "entry", InitFunc);
	Builder.SetInsertPoint(EntryBB);

	// Create the `module_info` instance in the initializer
	Value* ModInfo = Builder.CreateAlloca(ModuleInfoTy);

	// Set values in `module_info`    设置 moduleHash
	Builder.CreateStore(ConstantInt::get(Type::getInt64Ty(Context), g_ModueHashForPatch),
		Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 0));


	// Assume g_needPath_123456789 and functionCount are defined elsewhere    设置 g_needPathValueAddress地址
	GlobalVariable* GNeedPathVar_address = M.getGlobalVariable(__str_g_needpatch_lqcppReload + std::to_string(g_ModueHashForPatch), true);
	Value* GNeedPathVarPtr = GNeedPathVar_address
		? Builder.CreatePointerCast(GNeedPathVar_address, Type::getInt8PtrTy(Context))
		: Constant::getNullValue(Type::getInt8PtrTy(Context));

	Builder.CreateStore(GNeedPathVarPtr,
		Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 1));


	// Set values in `module_info`   设置 PatchedFunctionCount
	Builder.CreateStore(ConstantInt::get(Type::getInt32Ty(Context), g_needPatchFuncCount),
		Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 2));

#if !defUSeSymDataOffset
	//  设置 CollectAddressFunction 收集函数地址
	Function* ColFuncVarInfo = M.getFunction(__str_CollectAddressFunction + std::to_string(g_ModueHashForPatch));
	Value* ColFuncVarInfoPtr = ColFuncVarInfo
		? Builder.CreatePointerCast(ColFuncVarInfo, Type::getInt8PtrTy(Context))
		: Constant::getNullValue(Type::getInt8PtrTy(Context));

	Builder.CreateStore(ColFuncVarInfoPtr,
		Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 3));

	//设置 g_AddressCollectedCount
	Builder.CreateStore(ConstantInt::get(Type::getInt32Ty(Context), g_AddressCollectedCount),
		Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 4));
#endif

	////设置 collectFuncCounts
	//Builder.CreateStore(ConstantInt::get(Type::getInt32Ty(Context), 123456),
	//	Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 5));

	////设置 collectGvarCounts
	//Builder.CreateStore(ConstantInt::get(Type::getInt32Ty(Context), 567890),
	//	Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 6));

	// Call register_module_LQCppHotReload(&modInfo)
	Builder.CreateCall(RegisterFunc, { ModInfo });

	// Return from the initializer
	Builder.CreateRetVoid();

	// Add to global constructors to ensure it runs on module load
	appendToGlobalCtors(M, InitFunc, 0);

	errs() <<"**Leave " << __FUNCTION__ << " M:" << M.getName() << "\n";

	return true;//PreservedAnalyses::all();

}


//bool IsWrapperFunction(Function& F)
//{
//	std::string origFuncName = F.getName().str();
//	if (origFuncName.rfind(WrapperFuncStartStr, 0) == 0) //是否以WrapperFuncStartStr 开头
//	{
//		return true;
//	}
//
//	// 	if (origFuncName.rfind(__wrapper_GV_stuct_construct_function_prefix, 0) == 0) //不是wrapper函数全局变量struct的构造函数
//	// 	{
//	// 		return true;
//	// 	}
//	if (g_mapWrapperGVConstuctFunctionNames.find(origFuncName) != g_mapWrapperGVConstuctFunctionNames.end())
//	{
//		return true;
//	}
//	return false;
//}

//void generateWrapperBody(Function* WrapperFunc, Function* TargetFunc, LLVMContext& Context) {
//	IRBuilder<> Builder(BasicBlock::Create(Context, "entry", WrapperFunc));
//	auto ArgIter = WrapperFunc->arg_begin();
//	Value* runtime = ArgIter++;
//	Value* _ctx = ArgIter++;
//	Value* _sp = ArgIter++;
//	Value* _mem = ArgIter++;
//	Value* funAddress = ArgIter++;
//
//	// Setup return value if needed.   如果返回值不是空，则把调用函数的返回值设置到 *sp
//	Type* retType = TargetFunc->getReturnType();
//	if (!retType->isVoidTy()) {
//		if (retType->isIntegerTy(32)) {
//			// m3ApiReturnType (uint32_t)
//			Value* retPtr = Builder.CreateGEP(IntegerType::getInt8PtrTy(Context), _sp, Builder.getInt32(1));
//			Builder.CreateStore(Builder.getInt32(0), retPtr); // Placeholder for actual return value
//		}
//		else if (retType->isIntegerTy(64)) {
//			// m3ApiReturnType (uint64_t)
//			Value* retPtr = Builder.CreateGEP(IntegerType::getInt8PtrTy(Context), _sp, Builder.getInt32(1));
//			Builder.CreateStore(Builder.getInt64(0), retPtr); // Placeholder for actual return value
//		}
//	}
//
//	std::vector<Value*> Args;
//	uint32_t spIndex = 0;
//	for (auto& Arg : TargetFunc->args()) {
//		Type* ArgType = Arg.getType();
//		Value* ArgValue = nullptr;
//		if (ArgType->isPointerTy()) { //如果是指针类型，用* ((uint32_t *) (_sp++))获取偏移值，然后返回 (char*)_mem+offset的移值
//			// m3ApiGetArgMem
//			Value* Offset = Builder.CreateLoad(Builder.getInt32Ty(), Builder.CreateGEP(IntegerType::getInt8PtrTy(Context), _sp, Builder.getInt32(1)));
//			ArgValue = Builder.CreateGEP(IntegerType::getInt8PtrTy(Context), _mem, Offset);
//		}
//		else //如果是普通类型，直接返回 * ((TYPE *) (_sp++))
//		{
//			Value* funcIDValue = builder.getInt32(spIndex);
//			ArgValue = Builder.CreateLoad(ArgType, Builder.CreateGEP(IntegerType::getInt8PtrTy(Context), _sp, Builder.getInt32(0)));
//		}
//
//		++spIndex;
//		Args.push_back(ArgValue);
//	}
//
//	FunctionType* TargetFTy = TargetFunc->getFunctionType();
//	Value* Callee = Builder.CreateBitCast(funAddress, TargetFTy->getPointerTo());
//	Builder.CreateCall(TargetFTy, Callee, Args);
//
//	Builder.CreateRet(runtime);
//}

//void* func_UE(void* runtime, void* _ctx, uint64_t* _sp, void* _mem, void* funAddress)
//void generateWrapperBody(Function* WrapperFunc, Function* TargetFunc, LLVMContext& Context) {
//	IRBuilder<> Builder(BasicBlock::Create(Context, "entry", WrapperFunc));
//	auto ArgIter = WrapperFunc->arg_begin();
//	Value* runtime = ArgIter++;
//	Value* _ctx = ArgIter++;
//	Value* _sp = ArgIter++;
//	Value* _mem = ArgIter++;
//	Value* funAddress = ArgIter++;
//
//	uint32_t spIndex = 0; //sp读取指针
//	// Setup return value if needed.   如果返回值不是空，则把调用函数的返回值设置到 *sp
//	Type* retType = TargetFunc->getReturnType();
//	if (!retType->isVoidTy()) {
//		if (retType->isIntegerTy(32)) {
//			// m3ApiReturnType (uint32_t)
//			Value* retPtr = Builder.CreateGEP(IntegerType::getInt8PtrTy(Context), _sp, Builder.getInt32(1));
//			Builder.CreateStore(Builder.getInt32(0), retPtr); // Placeholder for actual return value
//		}
//		else if (retType->isIntegerTy(64)) {
//			// m3ApiReturnType (uint64_t)
//			Value* retPtr = Builder.CreateGEP(IntegerType::getInt8PtrTy(Context), _sp, Builder.getInt32(1));
//			Builder.CreateStore(Builder.getInt64(0), retPtr); // Placeholder for actual return value
//		}
//		spIndex++;
//	}
//
//	std::vector<Value*> Args;
//	for (auto& Arg : TargetFunc->args()) {
//		Type* ArgType = Arg.getType();
//		Value* ArgValue = nullptr;
//		if (ArgType->isPointerTy()) { //如果是指针类型，用* ((uint32_t *) (_sp++))获取偏移值，然后返回 (char*)_mem+offset的移值
//			// m3ApiGetArgMem
//			Value* SpIndexValue = Builder.getInt32(spIndex);
//			Value* Offset = Builder.CreateLoad(Builder.getInt32Ty(), Builder.CreateGEP(IntegerType::getInt32Ty(Context), _sp, SpIndexValue));
//			ArgValue = Builder.CreateGEP(IntegerType::getInt8PtrTy(Context), _mem, Offset);
//		}
//		else //如果是普通类型，直接返回 * ((TYPE *) (_sp++))
//		{
//			Value* SpIndexValue = Builder.getInt32(spIndex);
//			ArgValue = Builder.CreateLoad(ArgType, Builder.CreateGEP(ArgType, _sp, SpIndexValue));
//		}
//
//		++spIndex;
//		Args.push_back(ArgValue);
//	}
//
//	FunctionType* TargetFTy = TargetFunc->getFunctionType();
//	Value* Callee = Builder.CreateBitCast(funAddress, TargetFTy->getPointerTo());
//	//Builder.CreateCall(TargetFTy, Callee, Args);
//
//	if (!retType->isVoidTy()) {
//		// m3ApiReturnType
//		Value* CallResult = Builder.CreateCall(TargetFTy, Callee, Args);
//		Builder.CreateStore(CallResult, Builder.CreateGEP(retType, _sp, Builder.getInt32(0))); //设置返回值到最初的*sp 上
//	}
//	else {
//		Builder.CreateCall(TargetFTy, Callee, Args);
//	}
//
//	// Create the return instruction
//	// Create a global constant string (empty string here)
//	Value* EmptyStr = Builder.CreateGlobalStringPtr("");
//
//	// Return the empty string
//	Builder.CreateRet(EmptyStr); //m3ApiSuccess  return "";
//}

//std::set<std::string> g_mapWrapperGVConstuctFunctionNames;

// void createWrapperGlobalVariable(Module& M, std::string WrapperName, Function* WrapperFunc) {
//// 	// Create struct st_<WrapperName>
//// 	std::string StructName = "GVWrapperSt_" + WrapperName;
//// 	StructType* ST = StructType::create(M.getContext(), StructName);
//// 	ST->setBody({ Type::getInt64Ty(M.getContext()), Type::getInt8PtrTy(M.getContext()) });
//// 
//// 	// Create constructor for struct
//// 	FunctionType* ConstructorType = FunctionType::get(Type::getVoidTy(M.getContext()), false);
//// 	Function* ConstructorFunc = Function::Create(ConstructorType, Function::ExternalLinkage, "__ctor_"+ StructName/* __wrapper_GV_stuct_construct_function_prefix + WrapperName*/, M);
//// 
//// 	g_mapWrapperGVConstuctFunctionNames.insert("__ctor_" + StructName);
//// 
//// 	BasicBlock* ConstructorBB = BasicBlock::Create(M.getContext(), "entry", ConstructorFunc);
//// 	IRBuilder<> Builder(ConstructorBB);
//// 
//// 	uint64 WrapperNameHash = CityHash64(WrapperName.c_str(), WrapperName.size());
//// 	Constant* ArgWrapperNameHash = ConstantInt::get(Type::getInt64Ty(M.getContext()), WrapperNameHash);
//// 	Constant* FuncPtr = ConstantExpr::getBitCast(WrapperFunc, Type::getInt8PtrTy(M.getContext()));
//// 	Value* Args[] = { ArgWrapperNameHash, FuncPtr };
//// 	Builder.CreateCall(M.getOrInsertFunction("register_wrapper_func", Type::getVoidTy(M.getContext()), Type::getInt64Ty(M.getContext()), Type::getInt8PtrTy(M.getContext())), Args);
//// 	Builder.CreateRetVoid();
//// 
//// 	GlobalVariable* GVar = new GlobalVariable(M, ST, false,
//// 		GlobalValue::ExternalLinkage, nullptr, StructName);
//// 
//////  	// Add COMDAT to GVar
////	Comdat* C = M.getOrInsertComdat("__ctor_" + StructName);
////	C->setSelectionKind(Comdat::Any);
////	ConstructorFunc->setComdat(C);
////
////	appendToGlobalCtors(M, ConstructorFunc, 65535);
////
////
////	Comdat* CGVB = M.getOrInsertComdat(StructName);
////	CGVB->setSelectionKind(Comdat::Any);
////	GVar->setComdat(CGVB);
//// 
//// 	GVar->setInitializer(ConstantAggregateZero::get(ST));
//
/////////////////////////////////////////////////可以调用但是未解决重复问题
//	//std::string StructName = "GVWrapperSt_" + WrapperName;
//
//	// // 创建一个简单的函数类型，void function(void)
//	//FunctionType* FuncType = FunctionType::get(Type::getVoidTy(M.getContext()), false);
//	//Function* CtorFunc = Function::Create(FuncType, Function::ExternalLinkage, "__ctor_" + StructName, M);
//
//	//g_mapWrapperGVConstuctFunctionNames.insert("__ctor_" + StructName);
//
//	//// 在构造函数中添加简单的打印逻辑（需本机支持 printf 函数）
//	//BasicBlock* BB = BasicBlock::Create(M.getContext(), "entry", CtorFunc);
//	//IRBuilder<> Builder(BB);
//
//	//uint64 WrapperNameHash = CityHash64(WrapperName.c_str(), WrapperName.size());
//	//Constant* ArgWrapperNameHash = ConstantInt::get(Type::getInt64Ty(M.getContext()), WrapperNameHash);
//	//Constant* FuncPtr = ConstantExpr::getBitCast(WrapperFunc, Type::getInt8PtrTy(M.getContext()));
//
//	//Value* Args[] = { ArgWrapperNameHash, FuncPtr };
//	//Builder.CreateCall(M.getOrInsertFunction("register_wrapper_func", Type::getVoidTy(M.getContext()), Type::getInt64Ty(M.getContext()), Type::getInt8PtrTy(M.getContext())), Args);
//	//Builder.CreateRetVoid();
//
//
//	//// 为函数设置 COMDAT 属性
//	//CtorFunc->setComdat(M.getOrInsertComdat(CtorFunc->getName()));
//
//	//// 将函数添加到全局构造函数列表
//	//appendToGlobalCtors(M, CtorFunc, 0);  //未解决重复问题
//	/////////////////////////////////////////////////可以调用但是未解决重复问题
//
/////////////////////////////////////////////////可以调用但是未解决重复问题----增加函数名参数，定位问题-------------未解决重复问题
//	std::string StructName = "GVWrapperSt_" + WrapperName;
//
//	// 创建一个简单的函数类型，void function(void)
//	FunctionType* FuncType = FunctionType::get(Type::getVoidTy(M.getContext()), false);
//	Function* CtorFunc = Function::Create(FuncType, Function::ExternalLinkage, "__ctor_" + StructName, M);
//
//	g_mapWrapperGVConstuctFunctionNames.insert("__ctor_" + StructName);
//
//	// 在构造函数中添加简单的打印逻辑（需本机支持 printf 函数）
//	BasicBlock* BB = BasicBlock::Create(M.getContext(), "entry", CtorFunc);
//	IRBuilder<> Builder(BB);
//
//	uint64 WrapperNameHash = CityHash64(WrapperName.c_str(), WrapperName.size());
//	Constant* ArgWrapperNameHash = ConstantInt::get(Type::getInt64Ty(M.getContext()), WrapperNameHash);
//	Constant* FuncPtr = ConstantExpr::getBitCast(WrapperFunc, Type::getInt8PtrTy(M.getContext()));
//
//	Value* FormatStr = Builder.CreateGlobalStringPtr(WrapperName);
//
//	Value* Args[] = { ArgWrapperNameHash, FuncPtr,  FormatStr  };
//	Builder.CreateCall(M.getOrInsertFunction("register_wrapper_func", Type::getVoidTy(M.getContext()), Type::getInt64Ty(M.getContext()), Type::getInt8PtrTy(M.getContext()), Type::getInt8PtrTy(M.getContext())), Args);
//	Builder.CreateRetVoid();
//
//
//	// 为函数设置 COMDAT 属性
//	CtorFunc->setComdat(M.getOrInsertComdat(CtorFunc->getName()));
//
//	// 将函数添加到全局构造函数列表
//	appendToGlobalCtors(M, CtorFunc, 0);  //未解决重复问题
//	///////////////////////////////////////////////可以调用但是未解决重复问题----增加函数名参数，定位问题----------------未解决重复问题
//
// }


//bool helper::BuildWrapperFunction(Module& M)
//{
//	errs() << "\n" << "**Enter " << __FUNCTION__ << " M:" << M.getName() << " moduleHash:" << g_ModueHashForPatch << "\n";
//
//	LLVMContext& Context = M.getContext();
//	IRBuilder<> Builder(Context);
//	std::unordered_map<std::string, Function*> WrapperMap;
//
//	FunctionType* WrapperFTy = getWrapperFunctionType(Context);
//
//	for (Function& F : M.functions()) {
//		// 过滤掉声明或者不需要包装的函数
//		if (F.empty() || F.isDeclaration() || F.isIntrinsic())
//		{
//			errs() << "skip_func:" << F.getName() << "\n";
//			continue;
//		}
//
//		std::string Signature = getFunctionSignature(&F);
//		//Function* WrapperFunc = nullptr;
//
//		if (WrapperMap.find(Signature) == WrapperMap.end()) {
//
//// 			WrapperFunc = Function::Create(WrapperFTy, GlobalValue::ExternalLinkage, Signature, &M);
//// 
//// 			// 添加 COMDAT 属性
//// 			const std::string ComdatName = Signature + "_comdat";
//// 			M.getOrInsertComdat(ComdatName)->setSelectionKind(Comdat::Any);
//// 			WrapperFunc->setComdat(M.getOrInsertComdat(ComdatName));
//// 
//// 				// Create the function body
//// 			BasicBlock* EntryBB = BasicBlock::Create(Context, "entry", WrapperFunc);
//// 			IRBuilder<> Builder(EntryBB);
//// 
//// 			// return the runtime pointer
//// 			Builder.CreateRet(WrapperFunc->arg_begin());
//// 
//// 			WrapperMap[Signature] = WrapperFunc;
//			Function* WrapperFunc = Function::Create(WrapperFTy, GlobalValue::ExternalLinkage, Signature, &M);
//
//			// 使用相同的 Comdat 对象进行设置
//			Comdat* C = M.getOrInsertComdat(Signature);
//			C->setSelectionKind(Comdat::Any);
//			WrapperFunc->setComdat(C);
//
//			// Create the function body ---test body
//			//BasicBlock* EntryBB = BasicBlock::Create(Context, "entry", WrapperFunc);
//			//IRBuilder<> Builder(EntryBB);
//			//// return the runtime pointer
//			//Builder.CreateRet(WrapperFunc->arg_begin());
//			// Create the function body ---test body
//
//			generateWrapperBody(WrapperFunc, &F, Context);
//
//			// Create a COMDAT for this wrapper function
//			createWrapperGlobalVariable(M, Signature, WrapperFunc);
//
//			WrapperMap[Signature] = WrapperFunc;
//		}
//	}
//
//	errs() << "**Leave " << __FUNCTION__ << " M:" << M.getName() << "\n";
//	return true;
//}
