//===-- LqCppReload.h - Example Transformations ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_LqCppReload_H
#define LLVM_TRANSFORMS_UTILS_LqCppReload_H

#include "llvm/IR/PassManager.h"

namespace llvm {


class LqCppReloadModulePass : public PassInfoMixin<LqCppReloadModulePass> {
	raw_ostream& OS;
	std::string Banner;
	bool ShouldPreserveUseListOrder;
	bool EmitSummaryIndex;

public:
	LqCppReloadModulePass();
	LqCppReloadModulePass(raw_ostream& OS, const std::string& Banner = "",
		bool ShouldPreserveUseListOrder = false,
		bool EmitSummaryIndex = false);

	PreservedAnalyses run(Module& M, AnalysisManager<Module>&);
	//PreservedAnalyses TestRun(Module& M, AnalysisManager<Module>&);
	static bool isRequired() { return true; }
};



} // namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_LqCppReload_H
