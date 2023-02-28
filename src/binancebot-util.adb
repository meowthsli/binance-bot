with Ada.Numerics.Elementary_Functions;
with Ada.Float_Text_IO;
with Ada.Characters.Conversions;

package body Binancebot.Util is

   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String renames
     League.Strings.To_Universal_String;
   function Isclose
     (Input1 : Long_Float; Input2 : Long_Float; Deviation : Long_Float) return Boolean
   is
   begin
      return true when (((Input2 + Deviation) < (Input1 - Deviation)) and ((Input1 - Deviation) < Input2));
      return False;
   end Isclose;

   function f2s (F : in Float; Cnt : in Integer) return String is
      use Ada.Numerics.Elementary_Functions;
   begin
      --Ada.Text_IO.Put_Line(N'Image);
      --Ada.Float_Text_IO.Put(Item => F, Exp => 0, Aft => 8);
      --Ada.Text_IO.New_Line;
      if F > 1.0 then
         declare
            N : Integer :=
              Integer (Float'Floor (Log (Float'Floor (abs F), 10.0))) + 1 +
              Cnt + 1;
            Buffer : String (1 .. N);
         begin
            Ada.Float_Text_IO.Put
              (To => Buffer, Item => F, Exp => 0, Aft => Cnt);

            return Buffer;
         end;
      else
         declare
            N      : Integer := 1 + 1 + Cnt;
            Buffer : String (1 .. N);
         begin
            Ada.Float_Text_IO.Put
              (To => Buffer, Item => F, Exp => 0, Aft => Cnt);

            return Buffer;
         end;
      end if;
   end f2s;


   function f2s (E : in Long_Float; Cnt : in Integer) return String is
      use Ada.Numerics.Elementary_Functions;

      F : Float := Float(E);
   begin
      --Ada.Text_IO.Put_Line(N'Image);
      --Ada.Float_Text_IO.Put(Item => F, Exp => 0, Aft => 8);
      --Ada.Text_IO.New_Line;
      if F > 1.0 then
         declare
            N : Integer :=
              Integer (Float'Floor (Log (Float'Floor (abs F), 10.0))) + 1 +
              Cnt + 1;
            Buffer : String (1 .. N);
         begin
            Ada.Float_Text_IO.Put
              (To => Buffer, Item => F, Exp => 0, Aft => Cnt);

            return Buffer;
         end;
      else
         declare
            N      : Integer := 1 + 1 + Cnt;
            Buffer : String (1 .. N);
         begin
            Ada.Float_Text_IO.Put
              (To => Buffer, Item => F, Exp => 0, Aft => Cnt);

            return Buffer;
         end;
      end if;
   end f2s;


   function U2F (U : in League.Strings.Universal_String) return Long_Float is
   begin
      return Long_Float'Wide_Wide_Value(U.To_Wide_Wide_String);
   end;

   function U2S(Input : League.Strings.Universal_String) return String
   is
      use Ada.Characters.Conversions;
   begin
      return To_String(Input.To_Wide_Wide_String);
   end U2S;

   function Find_Multiple(Item : FFloat;
                          Step : FFloat) return FFloat is
      N_Item : FFloat := Item;
   begin
      loop
         return N_Item when (Long_Long_Integer(N_Item*100000000000)-Long_Long_Integer(Step*100000000000)) mod Long_Long_Integer(Step*100000000000) = 0;
         N_Item := N_Item - 0.0000000001;
      end loop;

      return 0.0;
   end;

end Binancebot.Util;
