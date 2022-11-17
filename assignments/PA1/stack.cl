(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class StackValue {
   type : String;
   number : Int;

   str2value(cmd : String): StackValue {
      let value : StackValue <- (new StackValue) in {
              if (cmd = "+") then {
                 value.settype("+");
              } else
                 if (cmd = "s") then {
                    value.settype("s");
                 } else
                    if (cmd = "e") then {
                       value.settype("e");
                    } else
                       if (cmd = "d") then {
                          value.settype("d");
                       } else
                          if (cmd = "x") then {
                             value.settype("x");
                          } else { -- int的情况
                             value.settype("int");
                             value.setnumber((new A2I).a2i_aux(cmd));
                          } fi fi fi fi fi;
         value;
      }
   };

   getvalue(ty : String, num : Int) : StackValue {
     {
         type <- ty;
         num <- number;
         self;
     } 
   };

   gettype(): String {
      {
        type;
      }
   };

   getnumber(): Int {
      {
        number;
      }
   };
   
   getvalstr(): String {
      let ret : String in {
         if (type = "int") then {
            ret <- (new A2I).i2a(number);
         } else {
            ret <- type;
         } fi;
         ret;
      }
   };

   setnumber(num : Int) : Bool {
      {
        number <- num;
        true;
      }
   };

   settype(tp : String) : Bool{
      {
        type <- tp;
        true;
      }
   };

   initvalue(tp : String, num : Int) : StackValue {
      let val : StackValue in {
         val <- (new StackValue);
         val.setnumber(num);
         val.settype(tp);
         val;
      }
   };
};

class StackNode {
   value : StackValue;
   next  : StackNode;

   init(cmd : String, ne : StackNode): StackNode {
      let node : StackNode <- (new StackNode) in {
         node.setvalue((new StackValue).str2value(cmd));
         node.setnext(ne);
         node;
      }
   };
   getval(): StackValue {
      {
        value;
      }
   };

   getvalstr(): String {
      {
         value.getvalstr();
      }
   };

   getnext(): StackNode {
      {
        next;
      }
   };
   setnext(node : StackNode): Bool {
      {
        next <- node;
        true;
      }
   };
   setvalue(val : StackValue): Bool {
      {
        value <- val;
        true;
      }
   };
};

class Stack {
   top : StackNode;
   cnt : Int;

   test(): Bool {
       {
         false;
       }
   };

   init(): Stack {
     {
        cnt <- 0;
        self;
     }
   };

   push(cmd : String) : Bool {
      {
          cnt <- cnt + 1;
          let node : StackNode in {
            node <- (new StackNode).init(cmd, top);
            top <- node;
            node;
          };
          -- (new IO).out_string((new A2I).i2a(cnt));
          true;
      }
   };

   popadd(): StackNode {
      let node : StackNode in {
         let nodea : StackNode,nodeb : StackNode, stra :String, strb : String, sum : Int in {
            nodea <- top;
            nodeb <- nodea.getnext();
            stra <- nodea.getvalstr();
            strb <- nodeb.getvalstr();
            sum <- (new A2I).a2i_aux(stra) + (new A2I).a2i_aux(strb);
            top <- nodeb;
            cnt <- cnt - 1;
            nodeb.setvalue((new StackValue).initvalue("int",sum));
            sum;
         };
         node;
      }
   };

   popswap(): StackNode {
      let node : StackNode in {
            let nodea : StackNode,nodeb : StackNode, vala :StackValue, valb : StackValue, sum : Int in {
            nodea <- top;
            nodeb <- nodea.getnext();
            vala <- nodea.getval();
            valb <- nodeb.getval();
            nodea.setvalue(valb);
            nodeb.setvalue(vala);
            sum;
         };
         node;
      }
   };

   pop(): StackNode {
      let node : StackNode <- top in {
         if 0 < cnt then {
            if (node.getval().gettype() = "int") then {
               node;
            } else {
               cnt <- cnt - 1;
               node <- top;
               let toptype : String in {
                  toptype <- node.getval().gettype();
                  if (toptype = "+") then {
                     top <- top.getnext();
                     popadd();
                  } else if (toptype = "s") then {
                     top <- top.getnext();
                     popswap();
                  } else {
                     (new IO).out_string("123");
                  } fi fi;
                  toptype;
               };
            } fi;
         } else {
            node;
         } fi;
         node;
      }
   };

   empty() : Bool {
      {
        cnt = 0;
      }
   };

   show(): Bool {
      let ret : Bool in {
         if (cnt = 0) then { false;} else {
            let tmptop : StackNode <- top in {
               let i : Int <- cnt in {
                  while (0 < i) loop {
                     (new IO).out_string(tmptop.getvalstr());
                     (new IO).out_string("\n");
                     tmptop <- tmptop.getnext();
                     i <- i - 1;
                  } pool;
               };
               tmptop;
            };
         } fi;
         true;
      }
   };
};


class Main inherits IO {
   stack : Stack;   
   execcmd(cmd : String) : Bool {
      let ret : Bool <- true in {
        if (cmd = "x") then { 
         ret <- false;
        } else 
        if (cmd = "d") then { 
            stack.show();
        } else 
        if (cmd = "e") then { 
            stack.pop();
         } else {
            stack.push(cmd);
        } fi fi fi;
        ret;
      }
   };

   main() : Object {
       let command : String, flag : Bool <- true in {
         stack <- (new Stack).init();
         (new IO).out_string(">");
         while (flag = true) loop {
            command <- (new IO).in_string();
            if (execcmd(command) = false) then {
                flag <- false;
            } else {
                (new IO).out_string(">");
            } fi;
          } pool;
         false;
      }
   };

};
