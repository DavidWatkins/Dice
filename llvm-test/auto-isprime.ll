; ModuleID = 'isprime.bc'
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.9.0"

@.str = private unnamed_addr constant [17 x i8] c"%d is not prime\0A\00", align 1

; Function Attrs: nounwind ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %i = alloca i32, align 4
  store i32 0, i32* %1
  store i32 10, i32* %x, align 4
  store i32 2, i32* %i, align 4
  br label %2

; <label>:2                                       ; preds = %15, %0
  %3 = load i32, i32* %i, align 4
  %4 = load i32, i32* %x, align 4
  %5 = icmp slt i32 %3, %4
  br i1 %5, label %6, label %18

; <label>:6                                       ; preds = %2
  %7 = load i32, i32* %x, align 4
  %8 = load i32, i32* %i, align 4
  %9 = srem i32 %7, %8
  %10 = icmp eq i32 %9, 0
  br i1 %10, label %11, label %14

; <label>:11                                      ; preds = %6
  %12 = load i32, i32* %x, align 4
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str, i32 0, i32 0), i32 %12)
  br label %14

; <label>:14                                      ; preds = %11, %6
  br label %18
                                                  ; No predecessors!
  %16 = load i32, i32* %i, align 4
  %17 = add nsw i32 %16, 1
  store i32 %17, i32* %i, align 4
  br label %2

; <label>:18                                      ; preds = %14, %2
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"Apple LLVM version 6.0 (clang-600.0.56) (based on LLVM 3.5svn)"}
