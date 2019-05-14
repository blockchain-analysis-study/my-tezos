

(* 
类型定义

type 类型参数（可选的，即 泛型)   类型名 = 类型表达式
 *)

 type positive = int

 type 'a typeName = 'a * 'a

 (* 根据类型，显示定义变量 *)
 let args ： positive = 12 + 12

 (* 显示类型的定义函数 *)
 let myFunc (args: point)  = args +. 2.0


(* 定义具备显示类型的返回值函数 *)
let myFunc inArgs : point = args +. 2.0
(* 指定 函数表达式的类型 *)
let myFunc inArgs = （2.0 : float)

