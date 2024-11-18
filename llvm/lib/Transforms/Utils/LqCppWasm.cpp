//===-- HelloWorld.cpp - Example Transformations --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/LqCppWasm.h"

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

#include <iostream>
#include <fstream>
#include <map>

#define DEBUG_TYPE "runtime-function-call-counter"

using namespace llvm;


namespace helper
{
	bool MakeGlobalGVLocal(Module& module);

} // namespace helper

LqCppWasmModulePass::LqCppWasmModulePass() : OS(dbgs()) {}
LqCppWasmModulePass::LqCppWasmModulePass(raw_ostream& OS, const std::string& Banner,
	bool ShouldPreserveUseListOrder,
	bool EmitSummaryIndex)
	: OS(OS), Banner(Banner),
	ShouldPreserveUseListOrder(ShouldPreserveUseListOrder),
	EmitSummaryIndex(EmitSummaryIndex) {}


uint64_t g_ModueHashForLqCppWasm = 0;

PreservedAnalyses LqCppWasmModulePass::run(Module& M, ModuleAnalysisManager& AM) {
	std::string input_str =  M.getSourceFileName();

	// 使用 CityHash64 计算 64 位哈希值
	g_ModueHashForLqCppWasm = CityHash64(input_str.c_str(), input_str.size());

	errs() << "####### Start_IR_handle " << __FUNCTION__ << "  #######  M:" << M.getName() <<"  moduleHash:" << g_ModueHashForLqCppWasm <<"\n";

	bool changed = true;
	changed &= helper::MakeGlobalGVLocal(M);

	errs() << "\n" << "####### End_IR_handle " << __FUNCTION__ << "  #######  M:" << M.getName() << "\n\n\n";
	return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}



bool helper::MakeGlobalGVLocal(Module& M)
{
	//errs() << "\n" << "**Enter " << __FUNCTION__ << " M:" << M.getName() << " moduleHash:" << g_ModueHashForLqCppWasm <<"\n";
	LLVMContext& context = M.getContext();

	bool modified = false;

	for (GlobalVariable& GV : M.globals()) {


		//// 检查是否为外部链接的全局变量
		//if (GV.isDeclaration() && GV.hasExternalLinkage()) {
		//	// 将其转为内部链接（local）变量
		//	GV.setLinkage(GlobalValue::InternalLinkage);
		//	// 若需要初始化，可以在这里设置初始值
		//	// e.g., GV.setInitializer(Constant::getNullValue(GV.getType()->getElementType()));

		//	modified = true;
		//}

		if (GV.isDeclaration() && GV.hasExternalLinkage()) {
			// Change linkage to private
			GV.setLinkage(GlobalValue::InternalLinkage);

			// Set initializer to 0 if it has none
			if (!GV.hasInitializer()) {
				Type* Ty = GV.getValueType();
				Constant* ZeroInit = Constant::getNullValue(Ty);
				GV.setInitializer(ZeroInit);
				modified = true;

				errs() << __FUNCTION__ << "    changing to local GV Name:" << GV.getName() << "\n";

			}
		}


	}

	errs() << "**Leave " << __FUNCTION__ << " M:" << M.getName() << "\n";
	return modified;
}
