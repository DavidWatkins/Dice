Line 7 of Compile.ml:
(* Symbol table: Information about all the names in scope *)
type env = {
    class_index    : int StringMap.t; (* Index for each class *)
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }

- I would include an "include_index", however we should, in our parser, pull in the additional stuff, or we should pull in the additional files in the compile.ml. I recommend we email the professor to ask what he recommends
- We should add the rule for include to be a string_lit in between the parens not an ID


In line 45 of compile.ml (let rec expr)
- We need to add the additional expressions and what we are going to translate them to.

In line 62 (let rec stmt):
- Need to add statements from the parser

- Make sure we can support empty methods and empty classes?

Line 30
- Add additional built-in functions, like print, fopen, etc. (See LRM)



I wanted to try to compile a class example in LLVM. Here is what I compiled:
#include <stdio.h>


class Rectangle{
	int width, height;
	public :
		void set_values(int, int);
		int area() {return width * height;}
};

void Rectangle::set_values (int x, int y) {
	width = x;
	height = y;
}

int main() {
	Rectangle rect;
	rect.set_values(3,4);
	printf("Area: %d\n", rect.area());
	return 0;
}

This would be equivalently the following in Dice:
include("stdlib");

class Rectangle {
  public int width;
  public int height;

  public int area() {
    return this.width * this.height;
  }

  public void set_values(int x, int y) {
    this.width = x;
    this.height = y;
  }

  public void main(char[,] args) {
    int area;
    Rectangle r;
    char[] msg;

    r = Rectangle();
    r.set_values(3, 4);
    area = r.area();

    msg = "Area: ";
    msg = String.concat(msg, String.parseInt(area));
    msg = String.concat(msg, "\n");

    print(msg);
  }
}

This results in the following LLVM code:

; ModuleID = 'isprime.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.Rectangle = type { i32, i32 }

@.str = private unnamed_addr constant [10 x i8] c"Area: %d\0A\00", align 1

; Function Attrs: nounwind uwtable
define void @_ZN9Rectangle10set_valuesEii(%class.Rectangle* %this, i32 %x, i32 %y) #0 align 2 {
  %1 = alloca %class.Rectangle*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store %class.Rectangle* %this, %class.Rectangle** %1, align 8
  store i32 %x, i32* %2, align 4
  store i32 %y, i32* %3, align 4
  %4 = load %class.Rectangle** %1
  %5 = load i32* %2, align 4
  %6 = getelementptr inbounds %class.Rectangle* %4, i32 0, i32 0
  store i32 %5, i32* %6, align 4
  %7 = load i32* %3, align 4
  %8 = getelementptr inbounds %class.Rectangle* %4, i32 0, i32 1
  store i32 %7, i32* %8, align 4
  ret void
}

; Function Attrs: uwtable
define i32 @main() #1 {
  %1 = alloca i32, align 4
  %rect = alloca %class.Rectangle, align 4
  store i32 0, i32* %1
  call void @_ZN9Rectangle10set_valuesEii(%class.Rectangle* %rect, i32 3, i32 4)
  %2 = call i32 @_ZN9Rectangle4areaEv(%class.Rectangle* %rect)
  %3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([10 x i8]* @.str, i32 0, i32 0), i32 %2)
  ret i32 0
}

declare i32 @printf(i8*, ...) #2

; Function Attrs: nounwind uwtable
define linkonce_odr i32 @_ZN9Rectangle4areaEv(%class.Rectangle* %this) #0 align 2 {
  %1 = alloca %class.Rectangle*, align 8
  store %class.Rectangle* %this, %class.Rectangle** %1, align 8
  %2 = load %class.Rectangle** %1
  %3 = getelementptr inbounds %class.Rectangle* %2, i32 0, i32 0
  %4 = load i32* %3, align 4
  %5 = getelementptr inbounds %class.Rectangle* %2, i32 0, i32 1
  %6 = load i32* %5, align 4
  %7 = mul nsw i32 %4, %6
  ret i32 %7
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Ubuntu clang version 3.4-1ubuntu3 (tags/RELEASE_34/final) (based on LLVM 3.4)"}

- Notice that in order to create a class, there is a simple "%class.Rectangle = type { i32, i32 }" at the beginning. This will make it simple to add additional classes. We need to add directives like these to our Bytecode.ml file
