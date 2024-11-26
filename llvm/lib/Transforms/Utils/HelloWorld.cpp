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

	bool BuildFunctionWrappers(Module& module);
	bool BuildFunctionInstrument(Module& module);
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


	errs() << "*******Enter MkTestModulePass::run*******" << M.getName() << "\n";

	errs() << "minkee func: " << __FUNCTION__ << M.getName() << "\n";

	bool changed = true;
	//helper::BuildFunctionWrappers(M);
	changed &= helper::BuildFunctionInstrument(M);
	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}

//ok�ģ�����
bool helper::BuildFunctionInstrument(Module& M)
{
	LLVMContext& context = M.getContext();

	// ��ȡ����������
	unsigned maxFuncCount = M.getFunctionList().size();

	// ����ȫ������
	ArrayType* boolArrayType = ArrayType::get(Type::getInt1Ty(context), maxFuncCount);
	GlobalVariable* needPatchArray = new GlobalVariable(M, boolArrayType, false, GlobalValue::InternalLinkage, Constant::getNullValue(boolArrayType), "g_NeedPatch");

	// ��ÿ����������һ������ID
	unsigned funcID = 0;
	for (Function& F : M) {

		if (F.empty())
		{
			continue;
		}

		Module* M = F.getParent();
		LLVMContext& Ctx = M->getContext();
		IRBuilder<> builder(Ctx);

		// ����ȫ�ֱ��� g_test
		GlobalVariable* g_test = M->getGlobalVariable("g_test");
		if (!g_test) {
			g_test = new GlobalVariable(*M, IntegerType::getInt32Ty(Ctx), false, GlobalValue::ExternalLinkage, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 0), "g_test");
		}

		// ��ȡ�򴴽����� callTest
		Function* callVmFunc = M->getFunction("CallVMFunction");
		if (!callVmFunc) {
			FunctionType* callTestTy = FunctionType::get(Type::getVoidTy(Ctx), false);
			callVmFunc = Function::Create(callTestTy, Function::ExternalLinkage, "CallVMFunction", M);
		}

		// ��ÿ����������ڻ�����Ŀ�ʼ������һ���µĻ�����
		BasicBlock& entryBB = F.getEntryBlock();
		BasicBlock* patchBB = BasicBlock::Create(Ctx, "patchBB", &F, &entryBB);

		// ��patchBB�в���patch�߼�
		builder.SetInsertPoint(patchBB);
		Value* g_test_val = builder.CreateLoad(IntegerType::getInt32Ty(context), g_test);
		Value* cond = builder.CreateICmpNE(g_test_val, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 0));
		BasicBlock* returnBB = BasicBlock::Create(Ctx, "returnBB", &F);
		builder.CreateCondBr(cond, returnBB, &entryBB);

		// ��returnBB�в������callTest��ָ��ͷ���ָ��
		builder.SetInsertPoint(returnBB);
		builder.CreateCall(callVmFunc);

		if (F.getReturnType()->isVoidTy()) {
			builder.CreateRetVoid();
		}
		else {
			builder.CreateRet(Constant::getNullValue(F.getReturnType()));
		}
	}

	return true;
}

// 
// bool helper::BuildFunctionInstrument(Module& M)
// {
// 	LLVMContext& context = M.getContext();
// 
// 	// ��ȡ����������
// 	unsigned maxFuncCount = M.getFunctionList().size();
// 
// 	// ����ȫ������
// 	ArrayType* boolArrayType = ArrayType::get(Type::getInt1Ty(context), maxFuncCount);
// 	GlobalVariable* needPatchArray = new GlobalVariable(M, boolArrayType, false, GlobalValue::InternalLinkage, Constant::getNullValue(boolArrayType), "g_NeedPatch");
// 
// 	// ��ÿ����������һ������ID
// 	unsigned funcID = 0;
// 	for (Function& F : M) {
// 		// ��ȡԭ����������
// 		std::string origFuncName = F.getName().str();
// 
// 		// ����ID
// 		ConstantInt* funcIDValue = ConstantInt::get(Type::getInt32Ty(context), funcID);
// 
// 		// �ں���ǰ�������
// 		BasicBlock& entryBlock = F.getEntryBlock();
// 		//IRBuilder<> builder(&entryBlock, entryBlock.begin());
// 		Instruction* firstInst = entryBlock.getFirstNonPHIOrDbg();
// 		IRBuilder<> builder(firstInst);
// 
// 		//Value* zeroIndex = ConstantInt::get(Type::getInt32Ty(context), 0);
// 		//Value* indices[] = { zeroIndex, funcIDValue };
// 
// 		//Value* needPatchPtr = builder.CreateGEP(needPatchArray, indices);
// 		Value* needPatchPtr = builder.CreateGEP(boolArrayType, needPatchArray, { builder.getInt32(0), funcIDValue }); //ConstantInt::get(Type::getInt32Ty(context), funcIDValue)
// 
// 
// 
// 		Value* needPatchValue = builder.CreateLoad(IntegerType::getInt1Ty(context), needPatchPtr);
// 		Value* cond = builder.CreateICmpEQ(needPatchValue, ConstantInt::get(Type::getInt1Ty(context), 1));
// 		BasicBlock* patchBlock = BasicBlock::Create(context, "patch", &F);
// 		BasicBlock* continueBlock = BasicBlock::Create(context, "continue", &F);
// 		builder.CreateCondBr(cond, patchBlock, continueBlock);
// 
// 		// ��patch���в������
// 		builder.SetInsertPoint(patchBlock);
// 		FunctionType* callPFuncType = FunctionType::get(Type::getVoidTy(context), { Type::getInt8PtrTy(context)->getPointerTo(), Type::getInt32Ty(context) }, false);
// 		Function* callPFunc = M.getFunction("CallPFunc");
// 		Value* parameters = builder.CreateBitCast(&F, Type::getInt8PtrTy(context)->getPointerTo());
// 		Value* parameterCount = builder.getInt32(F.arg_size());
// 		builder.CreateCall(callPFuncType, callPFunc, { parameters, parameterCount });
// 		builder.CreateRet(ConstantInt::get(Type::getInt32Ty(context), 0));
// 		//builder.CreateBr(continueBlock);
// 
// 		// ���º�������ڿ�
// // 		entryBlock.getTerminator()->eraseFromParent();
// // 		builder.SetInsertPoint(entryBlock.getTerminator());
// // 		builder.CreateCondBr(cond, patchBlock, continueBlock);
// 
// 		builder.SetInsertPoint(continueBlock);
// 
// 
// 		// ���º���ID
// 		funcID++;
// 	}
// 
// 	return true;
// }

//bool helper::BuildFunctionInstrument(Module& M)
//{
//	LLVMContext& context = M.getContext();
//
//	// ��ȡ����������
//	unsigned maxFuncCount = M.getFunctionList().size();
//
//	// ����ȫ������
//	ArrayType* boolArrayType = ArrayType::get(Type::getInt1Ty(context), maxFuncCount);
//	GlobalVariable* needPatchArray = new GlobalVariable(M, boolArrayType, false, GlobalValue::InternalLinkage, Constant::getNullValue(boolArrayType), "g_NeedPatch");
//
//	// ��ÿ����������һ������ID
//	unsigned funcID = 0;
//	for (Function& F : M) {
//		// ��ȡԭ����������
//		std::string origFuncName = F.getName().str();
//
//		// ����ID
//		ConstantInt* funcIDValue = ConstantInt::get(Type::getInt32Ty(context), funcID);
//
//		// �ں���ǰ�������
//		BasicBlock& entryBlock = F.getEntryBlock();
//		IRBuilder<> builder(&entryBlock, entryBlock.begin());
//
//		//Value* zeroIndex = ConstantInt::get(Type::getInt32Ty(context), 0);
//		//Value* indices[] = { zeroIndex, funcIDValue };
//
//		//Value* needPatchPtr = builder.CreateGEP(needPatchArray, indices);
//		Value* needPatchPtr = builder.CreateGEP(boolArrayType, needPatchArray, { builder.getInt32(0), funcIDValue }); //ConstantInt::get(Type::getInt32Ty(context), funcIDValue)
//
//
//
//		Value* needPatchValue = builder.CreateLoad(IntegerType::getInt1Ty(context), needPatchPtr);
//		Value* cond = builder.CreateICmpEQ(needPatchValue, ConstantInt::get(Type::getInt1Ty(context), 1));
//		BasicBlock* patchBlock = BasicBlock::Create(context, "patch", &F);
//		BasicBlock* continueBlock = BasicBlock::Create(context, "continue", &F);
//		builder.CreateCondBr(cond, patchBlock, continueBlock);
//
//		// ��patch���в������
//		builder.SetInsertPoint(patchBlock);
//		FunctionType* callPFuncType = FunctionType::get(Type::getVoidTy(context), { Type::getInt8PtrTy(context)->getPointerTo(), Type::getInt32Ty(context) }, false);
//		Function* callPFunc = M.getFunction("CallPFunc");
//		Value* parameters = builder.CreateBitCast(&F, Type::getInt8PtrTy(context)->getPointerTo());
//		Value* parameterCount = builder.getInt32(F.arg_size());
//		builder.CreateCall(callPFuncType, callPFunc, { parameters, parameterCount });
//		builder.CreateBr(continueBlock);
//
//		// ���²����
//		builder.SetInsertPoint(continueBlock);
//
//		// ���º���ID
//		funcID++;
//	}
//
//	return true;
//
//}


//void * func_UE(IM3Runtime runtime, IM3ImportContext _ctx, uint64_t * _sp,  void * _mem)
bool helper::BuildFunctionWrappers(Module& module)
{
	//auto& context = module.getContext();

	// ���������б�
	std::vector<Function*> functionList;

	// ����ģ���е����к�������������ӵ������б���
	for (Function& F : module) {
		functionList.push_back(&F);
	}

	// ���������б�����ÿ������
	for (Function* F : functionList) 
	{
		errs() << "minkee func:" << __FUNCTION__ << ": test  " << F->getName() << "\n";

		if (F->isDeclaration())
		{
			continue;
		}

		LLVMContext& context = F->getContext();
		Module* module = F->getParent();

		// ��ȡԭ����������
		std::string origFuncName = F->getName().str();

		// �����º�������
		std::string newFuncName = origFuncName + "_UE";

		// ��ȡԭ����������
		FunctionType* origFuncType = F->getFunctionType();

		// �����º�������
		FunctionType* newFuncType = FunctionType::get(Type::getInt8PtrTy(context), { Type::getInt8PtrTy(context), Type::getInt8PtrTy(context), Type::getInt64PtrTy(context), Type::getInt8PtrTy(context) }, false);

		// ��ģ���д����º���
		Function* newFunc = Function::Create(newFuncType, Function::ExternalLinkage, newFuncName, module);

		// �����º����Ļ�����
		BasicBlock* entryBlock = BasicBlock::Create(context, "entry", newFunc);
		IRBuilder<> builder(entryBlock);

		// ��ȡ�º����Ĳ���
		Argument* runtimeArg = &*newFunc->arg_begin();
		Argument* ctxArg = &*(newFunc->arg_begin() + 1);
		Argument* spArg = &*(newFunc->arg_begin() + 2);
		Argument* memArg = &*(newFunc->arg_begin() + 3);

		// ���ݵ�ǰ�����Ĳ��������� _sp ָ���ȡ����ֵ
		unsigned paramCount = F->arg_size();
		std::vector<Value*> params;
		for (unsigned i = 0; i < paramCount; ++i) {
			Value* spPtr = builder.CreateGEP(IntegerType::getInt64Ty(context), spArg, ConstantInt::get(Type::getInt32Ty(context), i+1));
			//Value* param = builder.CreateLoad(IntegerType::getInt64Ty(context), spPtr);

			Argument* arg = F->getArg(i);

			Value* param = builder.CreateLoad(arg->getType(), spPtr);
			params.push_back(param);
		}

		// ����ԭ����
		Value* callResult = builder.CreateCall(F, params);

		// ��������ָ��
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

//ok�ģ�����
//bool helper::BuildFunctionInstrument(Module& M)
//{
//	LLVMContext& context = M.getContext();
//
//	// ��ȡ����������
//	unsigned maxFuncCount = M.getFunctionList().size();
//
//	// ����ȫ������
//	ArrayType* boolArrayType = ArrayType::get(Type::getInt1Ty(context), maxFuncCount);
//	GlobalVariable* needPatchArray = new GlobalVariable(M, boolArrayType, false, GlobalValue::InternalLinkage, Constant::getNullValue(boolArrayType), "g_NeedPatch");
//
//	// ��ÿ����������һ������ID
//	unsigned funcID = 0;
//	for (Function& F : M) {
//
//		if (F.empty())
//		{
//			continue;
//		}
//
//		Module* M = F.getParent();
//		LLVMContext& Ctx = M->getContext();
//		IRBuilder<> builder(Ctx);
//
//		// ����ȫ�ֱ��� g_test
//		GlobalVariable* g_test = M->getGlobalVariable("g_test");
//		if (!g_test) {
//			g_test = new GlobalVariable(*M, IntegerType::getInt32Ty(Ctx), false, GlobalValue::ExternalLinkage, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 0), "g_test");
//		}
//
//		// ��ȡ�򴴽����� callTest
//		Function* callTest = M->getFunction("callTest");
//		if (!callTest) {
//			FunctionType* callTestTy = FunctionType::get(Type::getVoidTy(Ctx), false);
//			callTest = Function::Create(callTestTy, Function::ExternalLinkage, "callTest", M);
//		}
//
//		// ��ÿ����������ڻ�����Ŀ�ʼ������һ���µĻ�����
//		BasicBlock& entryBB = F.getEntryBlock();
//		BasicBlock* patchBB = BasicBlock::Create(Ctx, "patchBB", &F, &entryBB);
//
//		// ��patchBB�в���patch�߼�
//		builder.SetInsertPoint(patchBB);
//		Value* g_test_val = builder.CreateLoad(IntegerType::getInt32Ty(context), g_test);
//		Value* cond = builder.CreateICmpNE(g_test_val, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 0));
//		BasicBlock* returnBB = BasicBlock::Create(Ctx, "returnBB", &F);
//		builder.CreateCondBr(cond, returnBB, &entryBB);
//
//		// ��returnBB�в������callTest��ָ��ͷ���ָ��
//		builder.SetInsertPoint(returnBB);
//		builder.CreateCall(callTest);
//		if (F.getReturnType()->isVoidTy()) {
//			builder.CreateRetVoid();
//		}
//		else {
//			builder.CreateRet(Constant::getNullValue(F.getReturnType()));
//		}
//	}
//
//	return true;
//}