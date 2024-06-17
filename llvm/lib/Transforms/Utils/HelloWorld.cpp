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

#include <iostream>
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
	bool CountFunctionCallsInModule(Module& module);

	bool BuildWrapperFunction(Module& module);
	bool PatchFunctionCallVM(Module& module);
	bool GatherFunctionUseGValue(Module& module);
	bool CreateGetProcFunction(Module& module);
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

PreservedAnalyses MkTestModulePass::TestRun(Module& M, ModuleAnalysisManager& AM) {


	errs() << "*******Enter MkTestModulePass::run*******" << M.getName() << "\n";

	errs() << "minkee func: " << __FUNCTION__ << M.getName()<< "\n";

	bool changed = helper::CountFunctionCallsInModule(M);
	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();

}

PreservedAnalyses MkTestModulePass::run(Module& M, ModuleAnalysisManager& AM) {

	errs() << "*******Enter " << __FUNCTION__ << "  *******  M:"<<M.getName() << "\n";

	bool changed = true;
	changed &= helper::PatchFunctionCallVM(M);
	changed &= helper::BuildWrapperFunction(M);
			   helper::GatherFunctionUseGValue(M);
			   helper::CreateGetProcFunction(M);
	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}

// 计算字符串的哈希值
uint64_t hashFnv1a(const std::string& str) {
	const uint64_t prime = 1099511628211u;
	uint64_t hash = 14695981039346656037u;
	for (char c : str) {
		hash ^= static_cast<uint64_t>(c);
		hash *= prime;
	}
	return hash;
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

bool helper::CreateGetProcFunction(Module& M)
{
	errs() << "\n" << "*******Enter " << __FUNCTION__ << "  *******  M:" << M.getName() << "\n";

	LLVMContext& context = M.getContext();

	std::string filePath = M.getSourceFileName();
	std::string fileName = llvm::sys::path::filename(filePath).str();

	std::string funcName = "LQM_GetProc_" + fileName + "_" + llvm::utostr(hashFnv1a(fileName)).substr(0, 12);;

	errs() << "start Create func: " << funcName ;

	// 生成LQM_GetProc_fileName_hashID函数 int LQM_GetProc_fileName_hashID(void** guncAdd[]) 
	//FunctionType* funcType = FunctionType::get(Type::getInt32Ty(M.getContext()), { Type::getInt8PtrTy(M.getContext())->getPointerTo() }, false);
	//Function* LQM_GetProc = Function::Create(funcType, Function::ExternalLinkage, funcName, &M);
	FunctionType* funcType = FunctionType::get(Type::getInt32Ty(M.getContext()), { Type::getInt8PtrTy(M.getContext())->getPointerTo() }, false);
	Function* LQM_GetProc = Function::Create(funcType, Function::ExternalLinkage, funcName, &M);

////
	//BasicBlock* entryBB = BasicBlock::Create(M.getContext(), "entry", LQM_GetProc);
	//IRBuilder<> builder(entryBB);

	//size_t funcID = 0;
	//for (Function& F : M) {

	//	std::string origFuncName = F.getName().str();

	//	if (isWrapperFunc(F) && !F.isDeclaration()) {

	//		std::string origFuncName2 = F.getName().str();

	//		Constant* funcAddress = ConstantExpr::getBitCast(&F, Type::getInt8PtrTy(M.getContext()));
	//		Value* arrayElemPtr = builder.CreateConstGEP1_64(Type::getInt8PtrTy(M.getContext()), LQM_GetProc->getArg(0), funcID);
	//		builder.CreateStore(funcAddress, arrayElemPtr);
	//		++funcID;
	//	}
	//}

	//builder.CreateRet(builder.getInt32(funcID));
//////
	BasicBlock* entryBB = BasicBlock::Create(M.getContext(), "entry", LQM_GetProc);
	IRBuilder<> builder(entryBB);

	Value* guncAddIsNull = builder.CreateIsNull(LQM_GetProc->getArg(0));
	BasicBlock* returnCountBB = BasicBlock::Create(M.getContext(), "returnCount", LQM_GetProc);
	BasicBlock* storeFuncsBB = BasicBlock::Create(M.getContext(), "storeFuncs", LQM_GetProc);

	builder.CreateCondBr(guncAddIsNull, returnCountBB, storeFuncsBB);

	builder.SetInsertPoint(storeFuncsBB);
	size_t funcID = 0;
	for (Function& F : M) {
		if (isWrapperFunc(F) && !F.isDeclaration()) {
			Constant* funcAddress = ConstantExpr::getBitCast(&F, Type::getInt8PtrTy(M.getContext()));
			Value* arrayElemPtr = builder.CreateConstGEP1_64(Type::getInt8PtrTy(M.getContext()), LQM_GetProc->getArg(0), funcID);
			builder.CreateStore(funcAddress, arrayElemPtr);
			++funcID;
		}
	}
	builder.CreateBr(returnCountBB);

	builder.SetInsertPoint(returnCountBB);
	builder.CreateRet(builder.getInt32(funcID));



	return true;
}

bool helper::GatherFunctionUseGValue(Module& M)
{
	errs() << "\n" << "*******Enter " << __FUNCTION__ << "  *******  M:" << M.getName() << "\n";

	LLVMContext& context = M.getContext();

	// 获取函数总数量
	unsigned maxFuncCount = M.getFunctionList().size();

	// 给每个函数分配一个数字ID
	unsigned funcID = 0;
	for (Function& F : M) {

		if (F.empty())
		{
			continue;
		}

		// 创建一个集合来存储使用的全局变量
		std::set<GlobalVariable*> usedGlobals;

		for (BasicBlock& BB : F) {
			for (Instruction& I : BB) {
				if (auto* loadInst = dyn_cast<LoadInst>(&I)) {
					// 获取加载指令使用的全局变量
					Value* ptrOperand = loadInst->getPointerOperand();
					if (auto* globalVar = dyn_cast<GlobalVariable>(ptrOperand)) {
						usedGlobals.insert(globalVar);
					}
				}
			}
		}

		// 打印函数名和使用的全局变量
		if (usedGlobals.size())
		{
			errs() << "In_func: " << F.getName() << "\n";
			for (GlobalVariable* globalVar : usedGlobals) {
				errs() << "  Used Global Variable: " << globalVar->getName() << "\n";
			}
			errs() << "\n";
		}
	}

	return true;
}


bool helper::PatchFunctionCallVM(Module& M)
{
	errs() << "\n" << "*******Enter " << __FUNCTION__ << "  *******  M:" << M.getName() << "\n";
	LLVMContext& context = M.getContext();


	// 获取函数总数量
	unsigned maxFuncCount = M.getFunctionList().size();

	// 获取全局变量g_needPatch和g_funcAddress
	GlobalVariable* g_needPatch = M.getGlobalVariable("g_needPatch"); //char g_needPatch[]
	if (!g_needPatch) {
		// 如果g_needPatch不存在，则声明一个外部全局变量
		Type* i8Ty = Type::getInt8Ty(M.getContext());
		Type* i8PtrTy = PointerType::get(i8Ty, 0);
		g_needPatch = new GlobalVariable(M, i8PtrTy, false, GlobalValue::ExternalLinkage, nullptr, "g_needPatch");
	}


	//// 获取或创建全局变量 g_funcAddress
	//GlobalVariable* g_funcAddress = M.getGlobalVariable("g_funcAddress");
	//if (!g_funcAddress) {
	//	Type* voidPtrType = Type::getInt8PtrTy(context);
	//	// Create the global variable g_funcAddress
	//	g_funcAddress = new GlobalVariable(
	//		M,
	//		ArrayType::get(voidPtrType, M.getFunctionList().size()),
	//		false,
	//		GlobalValue::ExternalLinkage,
	//		nullptr,
	//		"g_funcAddress"
	//	);
	//}

// 	GlobalVariable* g_funcAddress = M.getGlobalVariable("g_funcAddress");
// 	Type* voidPtrTy = Type::getInt8PtrTy(M.getContext());
// 	uint64_t numFunctions = M.size();
// 	ArrayType* funcAddressArrayTy = ArrayType::get(voidPtrTy, numFunctions);
// 	if (!g_funcAddress)
// 	{
// 		 g_funcAddress = new GlobalVariable(M, funcAddressArrayTy, false,
// 			GlobalValue::ExternalLinkage, nullptr, "g_funcAddress");
// 	}


	// 创建CallVm函数原型 bool CallVm( /*char* strFunName,*/ void* pParameters, int paraCount, int funcID)    pParameters是栈上的原函数参数组成的数据
	FunctionType* callVmType = FunctionType::get(Type::getInt1Ty(context), { /*Type::getInt8PtrTy(context),*/ Type::getInt8PtrTy(context), Type::getInt32Ty(context), Type::getInt32Ty(context) }, false);
	Function* callVmFunc = Function::Create(callVmType, GlobalValue::ExternalLinkage, "LQCallVm", M);

	// 给每个函数分配一个数字ID
	int funcID = 0;

	for (Function& F : M) {

		if (F.empty())
		{
			continue;
		}


/////////////////////////////////////////////////////////////
//{
//			// 获取当前函数的地址
//			Constant* funcAddr = ConstantExpr::getBitCast(&F, Type::getInt8PtrTy(context));
//
//			// 创建一个全局变量引用，用于初始化 g_funcs 的元素
//			Constant* funcRef = ConstantExpr::getInBoundsGetElementPtr(g_funcAddress->getValueType(), g_funcAddress, { ConstantInt::get(Type::getInt32Ty(context), funcID) });
//
//			// 更新 g_funcs 的元素值为当前函数的地址
//			//g_funcAddress->setInitializer(funcRef->getAggregateElement(0U)->getType()->getElementType(), funcRef->getAggregateElement(0U));
//
//	   // 将函数的地址存储在g_funcs数组的相应位置
//			//g_funcAddress->setInitializer(ConstantArray::get(FuncPtrArrayTy, { ConstantExpr::getBitCast(&F, FuncPtrTy) }));
//
//			g_funcAddress->setInitializer(ConstantArray::get(funcAddressArrayTy, { funcPtr }));
//
//			// 为当前函数添加 funcID 属性
//			F.addFnAttr("funcID", std::to_string(funcID));
//}
/////////////////////////////////////////////////////////////

		F.addFnAttr("funcID", std::to_string(funcID));

		std::string origFuncName = F.getName().str();
		if (origFuncName == "CallVMFunction" || (origFuncName.npos != origFuncName.find("printf")) || (origFuncName.npos != origFuncName.find("main")))
		{
			errs() << "In_func:" << __FUNCTION__ << "  skip:" << origFuncName << "\n";
			continue;
		}

		errs() << "In_func:" << __FUNCTION__ << " **handle** f:" << origFuncName << "\n";

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

	// 获取g_needPatch[funcID]的值
		Value* funcIDValue = builder.getInt32(funcID);
		Value* needPatchPtr = builder.CreateGEP(IntegerType::getInt32Ty(context), g_needPatch, funcIDValue);
		Value* needPatchValue = builder.CreateLoad(IntegerType::getInt32Ty(context), needPatchPtr);

		Value* cond = builder.CreateICmpNE(needPatchValue, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 0)); //数值是0  1是测试数据
		BasicBlock* returnBB = BasicBlock::Create(Ctx, "PatchBody", &F);
		builder.CreateCondBr(cond, returnBB, &entryBB);

//PatchBody
		// 在PatchBody中插入调用call的指令和返回指令
		builder.SetInsertPoint(returnBB);
		//builder.CreateCall(callVmFunc);

		//patch 逻辑
		// 创建结构体实例并初始化
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

		++funcID;
	}

	return true;
}


//void * func_UE(IM3Runtime runtime, IM3ImportContext _ctx, uint64_t * _sp,  void * _mem)
bool helper::BuildWrapperFunction(Module& module)
{
	errs()<<"\n" << "*******Enter " << __FUNCTION__ << "  *******  M:" << module.getName() << "\n";
	//auto& context = module.getContext();

	// 创建函数列表
	std::vector<Function*> functionList;

	// 遍历模块中的所有函数，并将其添加到函数列表中
	for (Function& F : module) {
		functionList.push_back(&F);
	}

	// 遍历函数列表并处理每个函数
	for (Function* F : functionList) 
	{
		errs() << "func:" << __FUNCTION__ << "  F:" << F->getName() << "\n";

		if (F->isDeclaration() || F->isIntrinsic())
		{
			continue;
		}

		//F.addFnAttr("funcID", std::to_string(funcID));
		std::string FuncId = getFunctionID(*F);

		F->addFnAttr("funcID", FuncId);

		LLVMContext& context = F->getContext();
		Module* module = F->getParent();

		// 获取原函数的名称
		std::string origFuncName = F->getName().str();

		// 创建新函数名称
		std::string newFuncName = origFuncName + "_UE";

		// 获取原函数的类型
		FunctionType* origFuncType = F->getFunctionType();

		// 创建新函数类型
		FunctionType* newFuncType = FunctionType::get(Type::getInt8PtrTy(context), { Type::getInt8PtrTy(context), Type::getInt8PtrTy(context), Type::getInt64PtrTy(context), Type::getInt8PtrTy(context) }, false);

		// 在模块中创建新函数
		Function* newFunc = Function::Create(newFuncType, Function::ExternalLinkage, newFuncName, module);


		// 创建一个自定义属性，键为"IsWrapperFunc"，值为1
		Attribute IsWrapperFuncAttr = Attribute::get(F->getContext(), "IsWrapperFunc", "1");
		// 将自定义属性添加到函数
		newFunc->addFnAttr(IsWrapperFuncAttr);

		// 创建新函数的基本块
		BasicBlock* entryBlock = BasicBlock::Create(context, "entry", newFunc);
		IRBuilder<> builder(entryBlock);

		// 获取新函数的参数
		Argument* runtimeArg = &*newFunc->arg_begin();
		Argument* ctxArg = &*(newFunc->arg_begin() + 1);
		Argument* spArg = &*(newFunc->arg_begin() + 2);
		Argument* memArg = &*(newFunc->arg_begin() + 3);

		// 根据当前函数的参数个数从 _sp 指针获取参数值
		unsigned paramCount = F->arg_size();
		std::vector<Value*> params;
		for (unsigned i = 0; i < paramCount; ++i) {
			Value* spPtr = builder.CreateGEP(IntegerType::getInt64Ty(context), spArg, ConstantInt::get(Type::getInt32Ty(context), i+1));
			//Value* param = builder.CreateLoad(IntegerType::getInt64Ty(context), spPtr);

			Argument* arg = F->getArg(i);

			Value* param = builder.CreateLoad(arg->getType(), spPtr);
			params.push_back(param);
		}

		// 调用原函数
		Value* callResult = builder.CreateCall(F, params);

		// 创建返回指令
		//builder.CreateRet(callResult);
		builder.CreateRet(ConstantPointerNull::get(Type::getInt8PtrTy(context)));
	}

	return true;
}

bool helper::CountFunctionCallsInModule(Module& module)
{
	auto& context = module.getContext();

	// Function name to IR variable map that holds the call counters
	StringMap<Constant*> callCounterMap;

	// Function name to IR variable map that holds the function names
	StringMap<Constant*> functionNameMap;

	// Step 1. For each function in the module, inject the code for call-counting
	for (Function& function : module)
	{

		errs() << "minkee func:" << __FUNCTION__ << ":  " << function.getName() << "\n";

		if (function.isDeclaration())
		{
			continue;
		}

		// Get an IR builder and set the insertion point to the top of the function
		IRBuilder<> counterBuilder(&*function.getEntryBlock().getFirstInsertionPt());

		// Create a global variable to count the calls to this function
		std::string counterName = "counter_" + function.getName().str();
		Constant* counterVariable = helper::CreateGlobalVariable(module, counterName);
		callCounterMap[function.getName()] = counterVariable;

		// Create a global variable to hold the name of this function
		Constant* functionName = counterBuilder.CreateGlobalStringPtr(function.getName(), "name_" + function.getName());
		functionNameMap[function.getName()] = functionName;

		// Inject instruction to increment the call count each time this function executes
		LoadInst* counteCurrentValue = counterBuilder.CreateLoad(IntegerType::getInt32Ty(context), counterVariable);
		Value* counterNextValue = counterBuilder.CreateAdd(counterBuilder.getInt32(1), counteCurrentValue);
		counterBuilder.CreateStore(counterNextValue, counterVariable);

		// Let the opt tool print out some debug information
		// (Visible only if we pass "-debug" to the command and have an assert build.)
		LLVM_DEBUG(dbgs() << "Instrumented: " << function.getName() << "\n");
	}

	// Stop here if there is no function definition in this module
	if (callCounterMap.size() == 0)
	{
		return false;
	}

	// Step 2. Inject the declaration of "printf()"
	//
	// Create (or get) the following declaration in the IR module:
	//    declare i32 @printf(i8*, ...)
	//
	// It corresponds to the following C declaration:
	//    int printf(char*, ...)
	PointerType* printfArgType = PointerType::getUnqual(Type::getInt8Ty(context));
	FunctionType* printfFunctionType = FunctionType::get(IntegerType::getInt32Ty(context),
		printfArgType,
		/*IsVarArgs=*/true);
	FunctionCallee printfCallee = module.getOrInsertFunction("printf", printfFunctionType);

	// Step 3. Inject a global variable that will hold the printf format string
	Constant* formatString = ConstantDataArray::getString(context, "Function: %s, called %d times\n");
	Constant* formatStringVariable = module.getOrInsertGlobal("", formatString->getType());
	dyn_cast<GlobalVariable>(formatStringVariable)->setInitializer(formatString);

	// Step 4. Define a printf wrapper that will print the results
	//
	// Define `PrintfWrapper` that will print the results stored in functionNameMap
	// and callCounterMap. It is equivalent to the following C++ function:
	// ```
	//     void PrintfWrapper()
	//     {
	//          for (auto &item : functions)
	//          {
	//              printf("Function: %s, called %d times\n", item.name, item.count);
	//          }
	//     }
	// ```
	// ("item.name" comes from functionNameMap, "item.count" comes from callCounterMap.)
	FunctionType* printfWrapperType = FunctionType::get(Type::getVoidTy(context),
		{},
		/*IsVarArgs=*/false);
	Function* printfWrapperFunction =
		dyn_cast<Function>(module.getOrInsertFunction("PrintfWrapper", printfWrapperType).getCallee());
	BasicBlock* enterBlock = BasicBlock::Create(context, "enter", printfWrapperFunction);
	IRBuilder<> printfWrapperBuilder(enterBlock);

	Value* formatStringPtr = printfWrapperBuilder.CreatePointerCast(formatStringVariable, printfArgType);
	for (auto& item : callCounterMap)
	{
		Constant* functionName = functionNameMap[item.first()];
		LoadInst* counterValue = printfWrapperBuilder.CreateLoad(IntegerType::getInt32Ty(context), item.second);
		printfWrapperBuilder.CreateCall(printfCallee, { formatStringPtr, functionName, counterValue });
	}

	printfWrapperBuilder.CreateRetVoid();

	// Step 5. Call `PrintfWrapper` at the very end of this module
	appendToGlobalDtors(module, printfWrapperFunction, /*Priority=*/0);

	return true;
}
