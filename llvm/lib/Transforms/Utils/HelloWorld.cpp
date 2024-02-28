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

	// ��ȡ����������
	unsigned maxFuncCount = M.getFunctionList().size();

	// ��ÿ����������һ������ID
	unsigned funcID = 0;
	for (Function& F : M) {

		if (F.empty())
		{
			continue;
		}

		// ����һ���������洢ʹ�õ�ȫ�ֱ���
		std::set<GlobalVariable*> usedGlobals;

		for (BasicBlock& BB : F) {
			for (Instruction& I : BB) {
				if (auto* loadInst = dyn_cast<LoadInst>(&I)) {
					// ��ȡ����ָ��ʹ�õ�ȫ�ֱ���
					Value* ptrOperand = loadInst->getPointerOperand();
					if (auto* globalVar = dyn_cast<GlobalVariable>(ptrOperand)) {
						usedGlobals.insert(globalVar);
					}
				}
			}
		}

		// ��ӡ��������ʹ�õ�ȫ�ֱ���
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


	// ��ȡ����������
	unsigned maxFuncCount = M.getFunctionList().size();

	// ��ȡȫ�ֱ���g_needPatch��g_funcAddress
	GlobalVariable* g_needPatch = M.getGlobalVariable("g_needPatch"); //char g_needPatch[]
	if (!g_needPatch) {
		// ���g_needPatch�����ڣ�������һ���ⲿȫ�ֱ���
		Type* i8Ty = Type::getInt8Ty(M.getContext());
		Type* i8PtrTy = PointerType::get(i8Ty, 0);
		g_needPatch = new GlobalVariable(M, i8PtrTy, false, GlobalValue::ExternalLinkage, nullptr, "g_needPatch");
	}

	GlobalVariable* g_funcAddress = M.getGlobalVariable("g_funcAddress"); //void* g_funcAddress[]

	// ����CallVm����ԭ�� bool CallVm(char* strFunName, void* pParameters, int paraCount, int funcID)    pParameters��ջ�ϵ�ԭ����������ɵ�����
	FunctionType* callVmType = FunctionType::get(Type::getInt1Ty(context), { Type::getInt8PtrTy(context), Type::getInt8PtrTy(context), Type::getInt32Ty(context), Type::getInt32Ty(context) }, false);
	Function* callVmFunc = Function::Create(callVmType, GlobalValue::ExternalLinkage, "CallVm", M);


	// ��ÿ����������һ������ID
	int funcID = 0;

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

		// ��ȡ��ǰ�����Ĳ���
		std::vector<Value*> args;
		for (auto& arg : F.args()) {
			args.push_back(&arg);
		}

		// �����ṹ������ Paras�ǰѵ�ǰ�����Ĳ������浽һ��ջ�ṹ����, ��ͬ�������ܲ�һ��
		StructType* structTy = StructType::create(F.getContext(), "Paras");
		std::vector<Type*> structFields;
		for (auto arg : args) {
			structFields.push_back(arg->getType());
		}
		structTy->setBody(structFields);


		// ��ÿ����������ڻ�����Ŀ�ʼ������һ���µĻ�����
		BasicBlock& entryBB = F.getEntryBlock();
		BasicBlock* patchBB = BasicBlock::Create(Ctx, "patchBB", &F, &entryBB);

		// ��patchBB�в���patch�߼�
		builder.SetInsertPoint(patchBB);
		//Value* g_test_val = builder.CreateLoad(IntegerType::getInt32Ty(context), g_test);

	// ��ȡg_needPatch[funcID]��ֵ
		Value* funcIDValue = builder.getInt32(funcID);
		Value* needPatchPtr = builder.CreateGEP(IntegerType::getInt32Ty(context), g_needPatch, funcIDValue);
		Value* needPatchValue = builder.CreateLoad(IntegerType::getInt32Ty(context), needPatchPtr);

		Value* cond = builder.CreateICmpNE(needPatchValue, ConstantInt::get(IntegerType::getInt32Ty(Ctx), 0)); //��ֵ��0  1�ǲ�������
		BasicBlock* returnBB = BasicBlock::Create(Ctx, "PatchBody", &F);
		builder.CreateCondBr(cond, returnBB, &entryBB);

//PatchBody
		// ��PatchBody�в������call��ָ��ͷ���ָ��
		builder.SetInsertPoint(returnBB);
		//builder.CreateCall(callVmFunc);

		//patch �߼�
		// �����ṹ��ʵ������ʼ��
		Value* paras = builder.CreateAlloca(structTy);
		for (size_t i = 0; i < args.size(); ++i) {
			Value* field = builder.CreateStructGEP(structTy, paras, i);
			builder.CreateStore(args[i], field);
		}

		// ����CallVm����
		Value* funcName = builder.CreateGlobalStringPtr(F.getName()); //// ��ȡ��ǰ������
		Value* ArgCount = builder.getInt32(args.size());
		//Value* funcIDValue = builder.getInt32(funcID);
		Value* callVmArgs[] = { funcName, paras, ArgCount, funcIDValue };
		builder.CreateCall(callVmType, callVmFunc, callVmArgs);

		//return
		if (F.getReturnType()->isVoidTy()) {
			builder.CreateRetVoid();
		}
		else {
			builder.CreateRet(Constant::getNullValue(F.getReturnType()));
		}

		//patch �߼�
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

	// ���������б�
	std::vector<Function*> functionList;

	// ����ģ���е����к�������������ӵ������б���
	for (Function& F : module) {
		functionList.push_back(&F);
	}

	// ���������б�����ÿ������
	for (Function* F : functionList) 
	{
		errs() << "func:" << __FUNCTION__ << "  F:" << F->getName() << "\n";

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
