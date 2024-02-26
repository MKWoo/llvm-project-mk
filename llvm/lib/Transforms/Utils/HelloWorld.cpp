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
	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
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

// 	{ //测试代码，生成 CallVMFunction 函数定义
// 		// 创建函数类型
// 		FunctionType* funcType = FunctionType::get(Type::getVoidTy(context), false);
// 
// 		// 创建函数
// 		Function* func = Function::Create(funcType, Function::ExternalLinkage, "CallVMFunction", &M);
// 
// 		// 创建基本块和指令
// 		BasicBlock* entryBlock = BasicBlock::Create(context, "entry", func);
// 		IRBuilder<> builder(entryBlock);
// 
// 		// 创建 printf 函数调用
// 		FunctionType* printfType = FunctionType::get(Type::getInt32Ty(context), { Type::getInt8PtrTy(context) }, true);
// 		Function* printfFunc = M.getFunction("printf");//M.getOrInsertFunction("printf", printfType);
// 		Value* formatStr = builder.CreateGlobalStringPtr("in CallVMFunction\n");
// 		builder.CreateCall(printfFunc, formatStr);
// 
// 		// 创建返回指令
// 			builder.CreateRetVoid();
// 	}

{
		// 创建函数类型
		Type* int8PtrType = Type::getInt8PtrTy(context);
		FunctionType* funcType = FunctionType::get(Type::getVoidTy(context), { int8PtrType }, false);

		// 创建函数
		Function* func = Function::Create(funcType, Function::ExternalLinkage, "CallVMFunction", &M);

		// 创建基本块和指令
		BasicBlock* entryBlock = BasicBlock::Create(context, "entry", func);
		IRBuilder<> builder(entryBlock);

		// 创建 printf 函数调用
		FunctionType* printfType = FunctionType::get(Type::getInt32Ty(context), { int8PtrType }, true);
		Function* printfFunc = M.getFunction("printf");//M.getOrInsertFunction("printf", printfType);
		Value* formatStr = builder.CreateGlobalStringPtr("in CallVMFunction. str:%s\n");
		Value* arg = func->arg_begin();
		Value* args[] = { formatStr, arg };
		builder.CreateCall(printfFunc, args);
		// 创建返回指令
		builder.CreateRetVoid();
}


	// 获取函数总数量
	unsigned maxFuncCount = M.getFunctionList().size();

	// 创建全局数组
	ArrayType* boolArrayType = ArrayType::get(Type::getInt1Ty(context), maxFuncCount);
	GlobalVariable* needPatchArray = new GlobalVariable(M, boolArrayType, false, GlobalValue::InternalLinkage, Constant::getNullValue(boolArrayType), "g_NeedPatch");

	// 给每个函数分配一个数字ID
	unsigned funcID = 0;
	for (Function& F : M) {

		if (F.empty())
		{
			continue;
		}

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

		// 创建全局变量 g_test
		GlobalVariable* g_test = M->getGlobalVariable("g_test");
		if (!g_test) {
			g_test = new GlobalVariable(*M, IntegerType::getInt32Ty(Ctx), false, GlobalValue::ExternalLinkage, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 0), "g_test");
		}

// 		// 获取或创建函数 callTest
// 		Function* callVmFunc = M->getFunction("CallVMFunction");
// 		if (!callVmFunc) {
// 			FunctionType* callTestTy = FunctionType::get(Type::getVoidTy(Ctx), false);
// 			callVmFunc = Function::Create(callTestTy, Function::ExternalLinkage, "CallVMFunction", M);
// 		}
	  // 获取 CallVMFunction 函数
		Function* callVmFunc = M->getFunction("CallVMFunction");
		if (!callVmFunc) {
// 			FunctionType* callTestTy = FunctionType::get(Type::getVoidTy(Ctx), false);
// 			callVmFunc = Function::Create(callTestTy, Function::ExternalLinkage, "CallVMFunction", M);
		}

		// 在每个函数的入口基本块的开始处插入一个新的基本块
		BasicBlock& entryBB = F.getEntryBlock();
		BasicBlock* patchBB = BasicBlock::Create(Ctx, "patchBB", &F, &entryBB);

		// 在patchBB中插入patch逻辑
		builder.SetInsertPoint(patchBB);
		Value* g_test_val = builder.CreateLoad(IntegerType::getInt32Ty(context), g_test);
		Value* cond = builder.CreateICmpNE(g_test_val, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 1)); //数值是0  1是测试数据
		BasicBlock* returnBB = BasicBlock::Create(Ctx, "callVMRet", &F);
		builder.CreateCondBr(cond, returnBB, &entryBB);

		// 在callVMRet中插入调用call的指令和返回指令
		builder.SetInsertPoint(returnBB);
		//builder.CreateCall(callVmFunc);
// 
		// 获取当前函数名
		std::string funcName = F.getName().str();
		// 创建字符串常量
		Value* strFuncName = builder.CreateGlobalStringPtr(funcName);
		// 创建 CallVMFunction 函数调用

		Value* args[] = { strFuncName };
		builder.CreateCall(callVmFunc, args);

		//return
		if (F.getReturnType()->isVoidTy()) {
			builder.CreateRetVoid();
		}
		else {
			builder.CreateRet(Constant::getNullValue(F.getReturnType()));
		}
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

		if (F->isDeclaration())
		{
			continue;
		}

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
