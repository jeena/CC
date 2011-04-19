.class public t
.super java/lang/Object
.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
  .limit locals 1
  invokestatic t/main()I
  pop
  return
.end method

.method public static main()I
  .limit locals 0
  .limit stack 2
  invokestatic t/foo()V
  iconst_0
  ireturn
.end method

.method public static foo()V
  .limit locals 0
  .limit stack 8
  iconst_5
  iconst_3
  irem
  invokestatic Runtime/printInt(I)V
  ldc "\n"
  invokestatic Runtime/printString(Ljava/lang/String;)V
  return
  ldc2_w 10.0
  ldc2_w 5.0
  ddiv
  invokestatic Runtime/printDouble(D)V
  ldc "\n"
  invokestatic Runtime/printString(Ljava/lang/String;)V
  return
.end method


