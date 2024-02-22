//===-- HelloWorld.h - Example Transformations ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_LQCPPWASM_H
#define LLVM_TRANSFORMS_UTILS_LQCPPWASM_H

#include "llvm/IR/PassManager.h"

namespace llvm {




class LqCppWasmModulePass : public PassInfoMixin<LqCppWasmModulePass> {
	raw_ostream& OS;
	std::string Banner;
	bool ShouldPreserveUseListOrder;
	bool EmitSummaryIndex;

public:
	LqCppWasmModulePass();
	LqCppWasmModulePass(raw_ostream& OS, const std::string& Banner = "",
		bool ShouldPreserveUseListOrder = false,
		bool EmitSummaryIndex = false);

	PreservedAnalyses run(Module& M, AnalysisManager<Module>&);
	static bool isRequired() { return true; }
};



} // namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_LQCPPWASM_H
