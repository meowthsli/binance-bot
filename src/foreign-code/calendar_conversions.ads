------------------------------------------------------------------------------
--  Ada.Calendar.Time conversions library by Ivan "OCTAGRAM" Levashev       --
--  Russia, Altai Krai, Barnaul. 2016.                                      --
------------------------------------------------------------------------------
--                                                                          --
--  This is free and unencumbered software released into the public         --
--  domain.                                                                 --
--                                                                          --
--  Anyone is free to copy, modify, publish, use, compile, sell, or         --
--  distribute this software, either in source code form or as a compiled   --
--  binary, for any purpose, commercial or non-commercial, and by any       --
--  means.                                                                  --
--                                                                          --
--  In jurisdictions that recognize copyright laws, the author or authors   --
--  of this software dedicate any and all copyright interest in the         --
--  software to the public domain. We make this dedication for the benefit  --
--  of the public at large and to the detriment of our heirs and            --
--  successors. We intend this dedication to be an overt act of             --
--  relinquishment in perpetuity of all present and future rights to this   --
--  software under copyright law.                                           --
--                                                                          --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      --
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  --
--  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR       --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR   --
--  OTHER DEALINGS IN THE SOFTWARE.                                         --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Interfaces;

package Calendar_Conversions is

   --  Use -y in gnatbind to enable
   --
   --  In GNAT project:
   --  package Binder is
   --     for Default_Switches ("Ada") use
   --       ("-y");         -- leap seconds
   --  end Binder;
   --
   --  In gnatmake / gprmake / gprbuild:
   --
   --  gprbuild ... -bargs -y ...

   Leap_Support : constant Boolean;
   Unix_Epoch : constant Ada.Calendar.Time;

   -- Detect incorrect Ada.Calendar.Arithmetic."+" implementation
   Calendar_Arithmetic_Bug : constant Boolean;

   function Unix_Epoch_Plus (Days : Ada.Calendar.Arithmetic.Day_Count) return Ada.Calendar.Time;

   ----------------------------------
   -- Windows FILETIME conversions --
   ----------------------------------

   function Windows_Filetime_To_Ada_No_Leap (Item : Interfaces.Unsigned_64)
     return Ada.Calendar.Time;
   function Windows_Filetime_To_Ada_Leap (Item : Interfaces.Unsigned_64)
     return Ada.Calendar.Time with Pre => Leap_Support;
   function Ada_To_Windows_Filetime_No_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Unsigned_64;
   function Ada_To_Windows_Filetime_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Unsigned_64 with Pre => Leap_Support;

   -------------------------------------
   -- UNIX/POSIX.1 time_t conversions --
   -------------------------------------

   function Unix_Time_To_Ada_No_Leap (Item : Interfaces.Integer_64)
     return Ada.Calendar.Time;
   function Unix_Time_To_Ada_Leap (Item : Interfaces.Integer_64)
     return Ada.Calendar.Time with Pre => Leap_Support;
   function Ada_To_Unix_Time_No_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Integer_64;
   function Ada_To_Unix_Time_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Integer_64 with Pre => Leap_Support;

   -----------------------------------
   -- JavaScript Date() conversions --
   -----------------------------------

   function Javascript_Time_To_Ada_No_Leap (Item : Interfaces.Integer_64)
     return Ada.Calendar.Time;
   function Javascript_Time_To_Ada_Leap (Item : Interfaces.Integer_64)
     return Ada.Calendar.Time with Pre => Leap_Support;
   function Ada_To_Javascript_Time_No_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Integer_64;
   function Ada_To_Javascript_Time_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Integer_64 with Pre => Leap_Support;

   -------------------------------------------------------
   -- Formatting with leap second and time zone support --
   -------------------------------------------------------

   function Yaml_Value_With_Time_Zone (Date : String; Time_Zone : in out Ada.Calendar.Time_Zones.Time_Offset) return Ada.Calendar.Time;
   function Yaml_Value (Date : String; Default_Time_Zone : Ada.Calendar.Time_Zones.Time_Offset := 0) return Ada.Calendar.Time;

   function Yaml_Image
     (Date : Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset := 0;
      ISO_Format : Boolean := False;
      Include_Time_Zone : Boolean := False;
      Include_Time : Boolean := True;
      Include_Time_Fraction : Boolean := True)
      return String;

   function Yaml_Image_Local
     (Date : Ada.Calendar.Time;
      ISO_Format : Boolean := False;
      Include_Time_Fraction : Boolean := True)
     return String
   is
     (Yaml_Image
        (Date => Date,
         Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset (Date),
         ISO_Format => ISO_Format,
         Include_Time_Zone => True,
         Include_Time_Fraction => Include_Time_Fraction));

   function ISO_Image
     (Date : Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset := 0;
      Include_Time_Zone : Boolean := True;
      Include_Time : Boolean := True;
      Include_Time_Fraction : Boolean := True)
     return String
   is
     (Yaml_Image
        (Date => Date,
         Time_Zone => Time_Zone,
         ISO_Format => True,
         Include_Time_Zone => Include_Time_Zone,
         Include_Time => Include_Time,
         Include_Time_Fraction => Include_Time_Fraction));

private

   Flag : Integer;
   pragma Import (C, Flag, "__gl_leap_seconds_support");

   Leap_Support : constant Boolean := Flag = 1;

   Unix_Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Formatting.Time_Of (Year => 1970, Month => 1, Day => 1);

   use all type Ada.Calendar.Time;
   use all type Ada.Calendar.Arithmetic.Day_Count;

   Calendar_Arithmetic_Bug : constant Boolean := Leap_Support and then
     Unix_Epoch + 10_000 = Unix_Epoch + 864_000_000.0;

end Calendar_Conversions;
