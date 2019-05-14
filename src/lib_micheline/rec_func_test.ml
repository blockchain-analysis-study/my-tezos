

(* 
定义一个普通递归函数

list中元素相加


该函数的类型为：

val rec_sum : int list ->  int = <fun>
*)
let rec rec_sum lis = 

	match lis with
	(* 匹配到的是 头::尾 *)
	| hd::tl -> hd + (rec_sum tl)
	(* 匹配到的是 空集 *)
	| [] -> 0;;


(* 
改写上述函数为：包含尾递归的普通函数
尾递归是普通递归的优化

一般递归改成尾递归，都需要一个辅助参数

类型和上述一致

val list_sum : int list ->  int = <fun>
*)
let list_sum lis = 
	(* 定义尾递归 *)
	let rec sumfunc lis result = 
	match lis with
	(* 匹配到的是 头::尾 ,则把头部全部相加起来*)
	| hd::tl -> sumfunc tl (hd + result)
	(* 匹配到的是 空集 *)
	| [] -> result;;

	(* 这里先直接调用 sumfunc *)
in sumfunc lis 0;;




(* 声明一个引用 
   类型为 
   val my_ref : int ref = {contents = 0}	
*)

let my_ref = ref 0;;

(* 修改它的值
   为 
   - : unit = ()
*)
my_ref := 100;;


(* 读取还引用变量的值 
	为
	- : int = 100
*)

!my_ref;;



(* 
嵌套函数

其实就是 函数中的 函数
*)
(* 第一层 *)
let read_whole_channel chan =

	(* 这个 缩略名 buf 代表这 Buffer.create 4096 *)
	let buf = Buffer.create 4096 in
	let rec loop () =
	
	(*  
	【注意】

	把let ... in看作一条语句，永远不要在它后面加上单独的 ; 号。

	在所有代码块中其他的语句后面跟上一个单独的 ; 号，最后一个例外。


	; 是和+一样的运算符。当然只是概念上的，并不完全一样。
	+ 具有int -> int -> int类型 接受两个整型并返回一个整型（求和）。
	; 的类型是 unit -> 'b -> 'b 接受两个值并简单返回第二个。
	就像C中的,（逗号）运算符，你可以如同a + b + c + d一样写a ; b ; c ; d


	;不同于+的一个地方是不能像函数一样引用。如:

		(*
			let sum_list = List.fold_left ( + ) 0 // 这个是正确的

			let sum_list = List.fold_left ( ; ) 0 // 这个是错的
		*)
	
	*)

	let newline = input_line chan in

	(* buf 被这些地方使用 *)
	Buffer.add_string buf newline;
	Buffer.add_char buf '\n';
	loop ()
in
try
	loop ()
with
End_of_file -> Buffer.contents buf;;





(* 【一】 *)
let f a b =
	(a +. b) +. (a +. b) ** 2.
;;

(* 【二】 *)
let f a b =
	let x = a +. b in
	x +. x ** 2.
;;


(* 上述【二】 比 【一】 在某些编译器下还要快 *)





 (* 对模块进行重命名
	类似于 as 
	在引入一个嵌套模块（模块可以被嵌套）而又不想每次键入完整路径名的时候非常有用
*)

module Gr = Graphics;;

Gr.open_graph " 640x480";;
Gr.fill_circle 320 240 240;;
read_line ();;

(* 
在下面 5 种情况下可以省略 ;; 号


关键字let之前。
关键字open之前。
关键字type之前。
文件的最后。
一些其他（非常少）Ocaml能够“猜出”是语句结尾而不是中间的地方。



*)


open Random                   (* ;; *)
open Graphics;;

self_init ();;
open_graph " 640x480"         (* ;; 规则五 *)

let rec iterate r x_init i =
	if i = 1 then x_init
else
	let x = iterate r x_init (i-1) in
	r *. x *. (1.0 -. x);;

	for x = 0 to 639 do
	let r = 4.0 *. (float_of_int x) /. 640.0 in
	for i = 0 to 39 do
	let x_init = Random.float 1.0 in
	let x_final = iterate r x_init 500 in
	let y = int_of_float (x_final *. 480.) in
	Graphics.plot x y
done
done;;

read_line ()                  (* ;; 规则四 *)







(* 


?foo 和 ~foo 在OCaml中分别表示函数的可选和命名参数。
这个特性在C衍生的语言 中未必有对应的概念，
但是Perl，Python和Smalltalk都允许函数忽略某些参数，或者 以其他顺序传入参数。


foo#bar 是调用对象foo的bar方法。这和C++的foo->bar，Java的foo.bar， Perl的$foo->bar类似。


*)

open StdLabels
open GMain

(* 
这里声明了一个函数 file_dialog 
有两个命名参数: title 和 callback
以及一个 可选参数： filename
*)
let file_dialog ~title ~callback ?filename () =
	let sel =
	GWindow.file_selection ~title ~modal:true ?filename () in
	sel#cancel_button#connect#clicked ~callback:sel#destroy;
	sel#ok_button#connect#clicked ~callback:do_ok;
	sel#show ()




	(* 创建一个 map[string]string *)
	module MyUsers = Map.Make(String);;
	(* 现在已经创建了一个新的模块，叫做 MyUsers *)
  (* 首先，我们先 新建一个空的映射
	类型为:
	val m : 'a MyUsers.t = <abstr>
*)

let m = MyUsers.empty;;

  (* 再往里面加数据
	类型变化为:
	val m : string MyUsers.t = <abstr>
*)
let m = MyUsers.add "fred" "sugarplums" m;;
   (* 
   m是一个全新的映射，
   因此前一个m已经被隐藏掉。
   这个m比前一个多了用户 "fred" 和他的密码 "sugarplums"。

   有一点很值得指出的是，当我们加入字符串 "sugarplums" 的时候，
   我们已经固定了映射的目标类型。这也就是说，
   我们的模块MyUsers成为了一个只能从字符串到字符串 的映射。
   如果我们想插入一个整数作为键还是值，都必须创建一个新的映射。 
*)

(* 
定义一个函数，查看 map 中的数据
函数类型为:

val print_users : string -> string -> unit = <fun>
*)
let print_users key password =
	print_string(key ^ " " ^ password ^ "\n");;


(* 

使用该函数。

这里不明白为什么该函数 是个中间操作符 ?
*)

MyUsers.iter print_users m;;


(* 返回某个 key 对应的 value

- : string = "sugarplums"
*)
MyUsers.find "fred" m;;



 (* 
声明一个 Set 模块 ?
*)


module SS = Set.Make(String);;

(* 
新建一个空集 
类型为

val s : SS.t = <abstr>
*)

let s = SS.empty;;

(* 创建只包含一个元素的 集合，类型同上 *)
let s = SS.singleton "hello";;


(* 添加元素 *)

let s = List.fold_right SS.add ["hello"; "world"; "community"; "manager"; "stuff"; "blue"; "green"] s;;


(* 
通过remove函数移除某个元素。
使用filter 移除很多元素的时候
*)

(*
filter掉长度大于5的字符串
val my_filter : string -> bool = <fun>
*)

let my_filter str = String.length str <= 5;;
let s2 = SS.filter my_filter s;;

(* 或者 用匿名函数*)
let s2 = SS.filter (fun str -> String.length str <= 5) s;;

(* 查看某元素是否存在     - : bool = true *)
SS.mem "hello" s2;; 