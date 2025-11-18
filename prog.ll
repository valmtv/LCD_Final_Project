@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
define i32 @main() #0 {
0:
  %1 = icmp slt i32 1, 2
  br i1 %1, label %4, label %2
2:
  %3 = icmp sge i32 4, 4
  br label %4
4:
  %5 = phi i1 [1, %0], [%3, %2]
  %6 = call i32 (i8*, ...) @printf(i8* noundef @.str, i1 noundef %5)
  ret i32 0
}
declare i32 @printf(i8* noundef, ...) #1
