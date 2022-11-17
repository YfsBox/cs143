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

   gettype(): String {
      {
        "ok";
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
          true;
      }
   };

   pop(): StackNode {
      let node : StackNode in {
         if 0 < cnt then {
            cnt <- cnt - 1;
            node <- top;
            top <- top.getnext();
         } else {
            abort();
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
      {
        if (cmd = "x") then { abort();} else 
        if (cmd = "d") then { 
            stack.show();
        } else 
        if (cmd = "e") then { 
            (new IO).out_string("pop\n");
         } else {
            stack.push(cmd);
        } fi fi fi;
        true;
      }
   };

   main() : Object {
       let command : String in {
         stack <- (new Stack).init();
         while (true) loop {
            (new IO).out_string(">");
            command <- (new IO).in_string();
            execcmd(command);
          } pool;
         command;
      }
   };

};
