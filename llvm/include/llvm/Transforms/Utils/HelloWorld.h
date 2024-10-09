//===-- HelloWorld.h - Example Transformations ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_HELLOWORLD_H
#define LLVM_TRANSFORMS_UTILS_HELLOWORLD_H

#include "llvm/IR/PassManager.h"

namespace llvm {

class HelloWorldPass : public PassInfoMixin<HelloWorldPass> {
public:
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};




class MkTestModulePass : public PassInfoMixin<MkTestModulePass> {
	raw_ostream& OS;
	std::string Banner;
	bool ShouldPreserveUseListOrder;
	bool EmitSummaryIndex;

public:
	MkTestModulePass();
	MkTestModulePass(raw_ostream& OS, const std::string& Banner = "",
		bool ShouldPreserveUseListOrder = false,
		bool EmitSummaryIndex = false);

	PreservedAnalyses run(Module& M, AnalysisManager<Module>&);
	PreservedAnalyses TestRun(Module& M, AnalysisManager<Module>&);
	static bool isRequired() { return true; }
};



} // namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_HELLOWORLD_H
