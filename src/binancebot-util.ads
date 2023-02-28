with League.Strings;

with Binancebot.Parse;

package Binancebot.Util is

   type FFloat is delta 0.0000000001 digits 38;

   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String;
   function Isclose
     (Input1 : Long_Float; Input2 : Long_Float; Deviation : Long_Float) return Boolean;

   function f2s (F : in Float; Cnt : in Integer) return String;
   function f2s (E : in Long_Float; Cnt : in Integer) return String;
   function U2F (U : in League.Strings.Universal_String) return Long_Float;
   function U2S(Input : League.Strings.Universal_String) return String;

   function Find_Multiple(Item : FFloat;
                          Step : FFloat) return FFloat;


end Binancebot.Util;
