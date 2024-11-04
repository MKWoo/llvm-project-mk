//===-- HelloWorld.cpp - Example Transformations --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/HelloWorld.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/ADT/StringExtras.h"

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

PreservedAnalyses HelloWorldPass::run(Function &F,
                                      FunctionAnalysisManager &AM) {
  errs() << "    minkee   "  << F.getName() << "\n";
  return PreservedAnalyses::all();
}



namespace helper
{
	Constant* CreateGlobalVariable(Module& module, StringRef globalVariableName);

	bool PatchFunctionCallVM(Module& module);
	bool CreateCollectAddressFunction(Module& module);
	bool Create_Init_Module_Function(Module& module);

	bool BuildWrapperFunction(Module& module);


} // namespace helper

MkTestModulePass::MkTestModulePass() : OS(dbgs()) {}
MkTestModulePass::MkTestModulePass(raw_ostream& OS, const std::string& Banner,
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


PreservedAnalyses MkTestModulePass::TestRun(Module& M, ModuleAnalysisManager& AM) {


	errs() << "*******Enter MkTestModulePass::run*******" << M.getName() << "\n";

	errs() << "minkee func: " << __FUNCTION__ << M.getName()<< "\n";

// 	bool changed = helper::CountFunctionCallsInModule(M);
// 	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();

	return PreservedAnalyses::all();

}



uint64_t g_ModueHashForPatch = 0;
uint32_t  g_needPatchFuncCount =0;
uint32_t  g_AddressCollectedCount = 0; //收集的函数/全局变量地址  总个数，这个个数会写到结构体，注册module。 引擎new g_AddressCollectedCount个对象的数组，然后回调回来，进行收集

#define _str_LQCallVm "LQCallVm"

#define __str_g_needpatch_lqcppReload "g_needpatch_lqcppReload_"
#define __str_CollectAddressFunction "CollectAddressFunction_"

PreservedAnalyses MkTestModulePass::run(Module& M, ModuleAnalysisManager& AM) {
	std::string input_str =  M.getSourceFileName();

	// 使用 CityHash64 计算 64 位哈希值
	g_ModueHashForPatch = CityHash64(input_str.c_str(), input_str.size());

	errs() << "####### Start_IR_handle " << __FUNCTION__ << "  #######  M:" << M.getName() <<"  moduleHash:" << g_ModueHashForPatch <<"\n";

//	bool changed = true;
//	changed &= helper::PatchFunctionCallVM(M);
//	changed &= helper::CreateCollectAddressFunction(M);
//	changed &= helper::Create_Init_Module_Function(M);
//	changed &= helper::BuildWrapperFunction(M);
//	//changed &= helper::CreateCollectAddressFunction(M);
//// 	changed &= helper::BuildWrapperFunction(M);
//// 			   helper::CreateGetProcFunction(M);

	bool changed = true;
	changed &= helper::BuildWrapperFunction(M);
	changed &= helper::PatchFunctionCallVM(M);
	changed &= helper::CreateCollectAddressFunction(M);
	changed &= helper::Create_Init_Module_Function(M);

	errs() << "\n" << "####### End_IR_handle " << __FUNCTION__ << "  #######  M:" << M.getName() << "\n\n\n";
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

std::string getTypeName(Type* Ty) {


	if (Ty->getTypeID()>llvm::Type::TypeID::TargetExtTyID)
	{
		int*p =0;*p=0; //force crash
	}

	std::string strType = TypeID_str[Ty->getTypeID()];
	return strType;
}


#define WrapperFuncStartStr "wrapper_"

std::string getFunctionSignature(Function* F) {
	std::string Signature = WrapperFuncStartStr/*"wrapper_"*/ + getTypeName(F->getReturnType());
	for (auto& Arg : F->args()) {
		Signature += "_" + getTypeName(Arg.getType());
	}
	return Signature;
}

FunctionType* getWrapperFunctionType(LLVMContext& Context) {
	return FunctionType::get(Type::getInt8PtrTy(Context), {
		Type::getInt8PtrTy(Context), // runtime
		Type::getInt8PtrTy(Context), // _ctx
		Type::getInt64PtrTy(Context), // _sp
		Type::getInt8PtrTy(Context)  // _mem
		}, false);
}


bool helper::BuildWrapperFunction(Module& M)
{
	errs() << "\n" << "**Enter " << __FUNCTION__ << " M:" << M.getName() << " moduleHash:" << g_ModueHashForPatch << "\n";

	LLVMContext& Context = M.getContext();
	IRBuilder<> Builder(Context);
	std::unordered_map<std::string, Function*> WrapperMap;

	FunctionType* WrapperFTy = getWrapperFunctionType(Context);

	for (Function& F : M.functions()) {
		// 过滤掉声明或者不需要包装的函数
		if (F.empty() || F.isDeclaration() || F.isIntrinsic())
		{
			errs() << "skip_func:" << F.getName() << "\n";
			continue;
		}

		std::string Signature = getFunctionSignature(&F);
		//Function* WrapperFunc = nullptr;

		if (WrapperMap.find(Signature) == WrapperMap.end()) {

// 			WrapperFunc = Function::Create(WrapperFTy, GlobalValue::ExternalLinkage, Signature, &M);
// 
// 			// 添加 COMDAT 属性
// 			const std::string ComdatName = Signature + "_comdat";
// 			M.getOrInsertComdat(ComdatName)->setSelectionKind(Comdat::Any);
// 			WrapperFunc->setComdat(M.getOrInsertComdat(ComdatName));
// 
// 				// Create the function body
// 			BasicBlock* EntryBB = BasicBlock::Create(Context, "entry", WrapperFunc);
// 			IRBuilder<> Builder(EntryBB);
// 
// 			// return the runtime pointer
// 			Builder.CreateRet(WrapperFunc->arg_begin());
// 
// 			WrapperMap[Signature] = WrapperFunc;
			Function* WrapperFunc = Function::Create(WrapperFTy, GlobalValue::ExternalLinkage, Signature, &M);

			// 使用相同的 Comdat 对象进行设置
			Comdat* C = M.getOrInsertComdat(Signature);
			C->setSelectionKind(Comdat::Any);
			WrapperFunc->setComdat(C);

			// Create the function body
			BasicBlock* EntryBB = BasicBlock::Create(Context, "entry", WrapperFunc);
			IRBuilder<> Builder(EntryBB);

			// return the runtime pointer
			Builder.CreateRet(WrapperFunc->arg_begin());

			WrapperMap[Signature] = WrapperFunc;
		}
	}

	errs() << "**Leave " << __FUNCTION__ << " M:" << M.getName() << "\n";
	return true;
}


// 给每个函数分配一个数字ID //整个module里面的函数id是基于g_funcID递增的
int g_funcID = 0;

bool IsWrapperFunction(Function& F)
{
	std::string origFuncName = F.getName().str();
	if (origFuncName.rfind(WrapperFuncStartStr, 0) ==0)
	{
		return true;
	}
	return false;
}

bool helper::PatchFunctionCallVM(Module& M)
{
	errs() << "\n" << "**Enter " << __FUNCTION__ << " M:" << M.getName() << " moduleHash:" << g_ModueHashForPatch <<"\n";
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
	FunctionType* callVmType = FunctionType::get(Type::getInt32Ty(context), { /*Type::getInt8PtrTy(context),*/ Type::getInt64PtrTy(context), Type::getInt32Ty(context), Type::getInt32Ty(context) }, false);
	Function* callVmFunc = Function::Create(callVmType, GlobalValue::ExternalLinkage, _str_LQCallVm, M);

	for (Function& F : M) {

		if (F.empty() || F.isDeclaration() || F.isIntrinsic()  || IsWrapperFunction(F))
		{
			errs() << "skip_func:" << F.getName() << "\n";
			continue;
		}

		F.addFnAttr("funcID", std::to_string(g_funcID));

		std::string origFuncName = F.getName().str();
		if (origFuncName == "CallVMFunction" || (origFuncName.npos != origFuncName.find("printf")) || (origFuncName.npos != origFuncName.find("main")))
		{
			errs() << "skip by name func:" << origFuncName << "\n";
			continue;
		}

		errs() << "patch_function id:" << g_funcID << "  Name:"<< origFuncName << "\n";

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
		builder.CreateCall(callVmType, callVmFunc, callVmArgs);

		//return
		if (F.getReturnType()->isVoidTy()) {
			builder.CreateRetVoid();
		}
		else {
			builder.CreateRet(Constant::getNullValue(F.getReturnType()));
		}

		//patch 逻辑
//PatchBody

		++g_funcID;
	}
	errs() << "**Leave " << __FUNCTION__ << " M:" << M.getName() << "\n";
	return true;
}

//collect address
enum emCollectType
{
	em_type_function =0,
	em_type_globalValue = 1,
};


struct CollectItemInfo
{
	uint32_t collectIndex = 0; //在数组的存放index，调用Builder.CreateStore时index +1。 当前collectIndex的最大值决定回调CollectAddressFunction时，需要new的数组大小
	uint32_t collectType = 0; //函数或全局变量 emCollectType

	CollectItemInfo():collectIndex(0) ,collectType(0)
	{}
	CollectItemInfo(const CollectItemInfo& other):collectIndex(other.collectIndex) ,collectType(other.collectType)
	{}
	CollectItemInfo(uint32_t collectIndex_in, uint32_t collectType_in):collectIndex(collectIndex_in),collectType(collectType_in)
	{}
};


struct stSavedCollectInfo //序列化数据到本地
{
	uint64_t moduleHash;
	std::map<std::string, CollectItemInfo> mapCollectAddressData; //Name:?MinkeeTestFunc@@YA_NPEB_W@Z        patch_function id:109  //记录index 和名字关系，

	stSavedCollectInfo():moduleHash(0){}
};

stSavedCollectInfo g_collectInfo; //序列化数据到本地

// 文件锁定
std::mutex fileMutex;

// 保存到二进制文件（追加）
void saveToBinaryFile(const std::string& filename) {

	std::lock_guard<std::mutex> lock(fileMutex);

#if defined(__APPLE__)
	int fd = open(filename.c_str(), O_WRONLY | O_CREAT | O_APPEND, 0666);
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

	std::ofstream outFile(filename, std::ios::binary | std::ios::app);
	outFile.write(reinterpret_cast<const char*>(&g_collectInfo.moduleHash), sizeof(g_collectInfo.moduleHash));

	size_t mapSize = g_collectInfo.mapCollectAddressData.size();
	outFile.write(reinterpret_cast<const char*>(&mapSize), sizeof(mapSize));

	for (const auto& [key, value] : g_collectInfo.mapCollectAddressData) {
		size_t keySize = key.size();
		outFile.write(reinterpret_cast<const char*>(&keySize), sizeof(keySize));
		outFile.write(key.c_str(), keySize);
		outFile.write(reinterpret_cast<const char*>(&value.collectIndex), sizeof(value.collectIndex));
		outFile.write(reinterpret_cast<const char*>(&value.collectType), sizeof(value.collectType));
	}



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
		if (F.empty() || F.isDeclaration() || F.isIntrinsic() || (F.getName() == __str_CollectAddressFunction + std::to_string(g_ModueHashForPatch)) /*|| IsWrapperFunction(F)*/)
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

			CollectItemInfo oItemInfo(CollectIndex-1, em_type_function);
			g_collectInfo.mapCollectAddressData[F.getName().data()] = oItemInfo;

			errs() << "    Collect_local_func:" << F.getName() << "\n";
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

								CollectItemInfo oItemInfo(CollectIndex - 1, em_type_function);
								g_collectInfo.mapCollectAddressData[Callee->getName().data()] = oItemInfo;

								errs() << "    Collect_Call_func:" << Callee->getName() << "\n";
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

							CollectItemInfo oItemInfo(CollectIndex - 1, em_type_globalValue);
							g_collectInfo.mapCollectAddressData[GV->getName().data()] = oItemInfo;

							errs() << "    Collect_Used_GV:" << GV->getName() << "\n";
						}

					}
				}
			}
		}
	}

	g_AddressCollectedCount = CollectIndex;
	saveToBinaryFile("D:\\dev-game\\IOS-patch\\TestRunVm\\testCollectAddrData.dat");

	Builder.CreateRetVoid();

	errs() <<"**Leave " << __FUNCTION__ << " M:" << M.getName() << "\n";
	return true;
}


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
		Type::getInt8PtrTy(Context), // collect_FuncVar_Info //收集函数地址
		Type::getInt32Ty(Context), // collectCounts //收集的总数量
		Type::getInt32Ty(Context), // collectFuncCounts
		Type::getInt32Ty(Context)  // collectGvarCounts
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

	//设置 collectFuncCounts
	Builder.CreateStore(ConstantInt::get(Type::getInt32Ty(Context), 123456),
		Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 5));

	//设置 collectGvarCounts
	Builder.CreateStore(ConstantInt::get(Type::getInt32Ty(Context), 567890),
		Builder.CreateStructGEP(ModuleInfoTy, ModInfo, 6));

	// Call register_module_LQCppHotReload(&modInfo)
	Builder.CreateCall(RegisterFunc, { ModInfo });

	// Return from the initializer
	Builder.CreateRetVoid();

	// Add to global constructors to ensure it runs on module load
	appendToGlobalCtors(M, InitFunc, 0);

	errs() <<"**Leave " << __FUNCTION__ << " M:" << M.getName() << "\n";

	return true;//PreservedAnalyses::all();

}

