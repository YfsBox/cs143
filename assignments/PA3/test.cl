(*class A {
    avar : Int;
    aname : String;
    aint : Int <- 3;
};*)

class B {
        getvalue(ty : String, num : Int) : B {
         {
             type <- ty;
             num <- number;
             self;
         }
       };
};

(*class C inherits B {

};*)

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
};