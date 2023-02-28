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

with Ada.Calendar.Time_Zones;
with Ada.Strings.Fixed;

package body Calendar_Conversions is

   use all type Ada.Calendar.Time;
   use all type Ada.Calendar.Arithmetic.Day_Count;
   use all type Ada.Calendar.Time_Zones.Time_Offset;
   use all type Interfaces.Unsigned_64;
   use all type Interfaces.Integer_64;

   ---------------------
   -- Unix_Epoch_Plus --
   ---------------------

   function Unix_Epoch_Plus (Days : Ada.Calendar.Arithmetic.Day_Count) return Ada.Calendar.Time is
   begin
      return Result : Ada.Calendar.Time := Unix_Epoch + Days do
         if not Calendar_Arithmetic_Bug then
            return;
         end if;

         -- Some GNAT versions (2016 GPL included) only support leap seconds in
         -- Arithmetic.Difference, but not Arithmetic."+"
         Fix_GNAT_Bug : declare
            Result_Plus_Day : constant Ada.Calendar.Time := Result + 1;
            Days : Ada.Calendar.Arithmetic.Day_Count;
            Seconds : Duration;
            Leap_Seconds : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
         begin
            Difference (Result_Plus_Day, Unix_Epoch, Days, Seconds, Leap_Seconds);
            if Seconds /= 0.0 then
               -- pragma Assert (Leap_Seconds_Count < 86_400);
               -- Hopefully this issue will be fixed long before
               Result := Result_Plus_Day - Seconds;
            end if;
         end Fix_GNAT_Bug;
      end return;
   end Unix_Epoch_Plus;

   -----------
   -- Floor --
   -----------

   function Floor (Item : Duration) return Interfaces.Integer_64 is
   begin
      return Result : Interfaces.Integer_64 := Interfaces.Integer_64 (Item) do
         if Duration (Result) > Item then
            Result := Result - 1;
         end if;
      end return;
   end Floor;

   -------------------
   -- Natural_Floor --
   -------------------

   function Natural_Floor (Item : Duration) return Natural is
   begin
      return Result : Natural := Natural (Item) do
         if Duration (Result) > Item then
            Result := Result - 1;
         end if;
      end return;
   end Natural_Floor;

   Windows_Filetime_Day : constant := 24 * 60 * 60 * 10_000_000;

   -------------------------------------
   -- Windows_Filetime_To_Ada_No_Leap --
   -------------------------------------

   function Windows_Filetime_To_Ada_No_Leap (Item : Interfaces.Unsigned_64)
     return Ada.Calendar.Time
   is
      Item_From_Epoch : constant Interfaces.Integer_64 :=
        Interfaces.Unsigned_64'Pos (Item) - 11644473600_000_000_0;
      Moment_In_Day_64 : constant Interfaces.Integer_64 :=
        Item_From_Epoch mod Windows_Filetime_Day;
      Days : constant Ada.Calendar.Arithmetic.Day_Count := Ada.Calendar.Arithmetic.Day_Count
        ((Item_From_Epoch - Moment_In_Day_64) / Windows_Filetime_Day);
      Moment_In_Day : constant Duration :=
        Interfaces.Integer_64'Pos (Moment_In_Day_64) * Duration'(0.000_000_1);
   begin
      return Unix_Epoch_Plus (Days) + Moment_In_Day;
   end Windows_Filetime_To_Ada_No_Leap;

   ----------------------------------
   -- Windows_Filetime_To_Ada_Leap --
   ----------------------------------

   function Windows_Filetime_To_Ada_Leap (Item : Interfaces.Unsigned_64)
     return Ada.Calendar.Time
   is
      Item_From_Epoch : constant Duration :=
        (Interfaces.Unsigned_64'Pos (Item) - 11644473600_000_000_0) * Duration'(0.000_000_1);
   begin
      return Unix_Epoch + Item_From_Epoch;
   end Windows_Filetime_To_Ada_Leap;

   -------------------------------------
   -- Ada_To_Windows_Filetime_No_Leap --
   -------------------------------------
   
   function Ada_To_Windows_Filetime_No_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Unsigned_64
   is
      Days : Ada.Calendar.Arithmetic.Day_Count;
      Seconds : Duration;
      Integral_Seconds : Natural; -- divide into parts to avoid overflows
      Leap_Seconds : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
   begin
      Difference (Item, Unix_Epoch, Days, Seconds, Leap_Seconds);
      Integral_Seconds := Natural_Floor (Seconds);
      return
        11644473600_000_000_0 +
        Ada.Calendar.Arithmetic.Day_Count'Pos (Days) * Interfaces.Unsigned_64'(Windows_Filetime_Day) +
        Interfaces.Unsigned_64 (Integral_Seconds) * 10_000_000 +
        Interfaces.Unsigned_64 (Floor ((Seconds - Duration (Integral_Seconds)) * 10_000_000));
   end Ada_To_Windows_Filetime_No_Leap;

   ----------------------------------
   -- Ada_To_Windows_Filetime_Leap --
   ----------------------------------
   
   function Ada_To_Windows_Filetime_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Unsigned_64
   is
      Seconds_From_Epoch : constant Duration := Item - Unix_Epoch;
      Integral_Seconds : constant Interfaces.Integer_64 := Floor (Seconds_From_Epoch);
   begin
      return
        11644473600_000_000_0 +
        (Interfaces.Unsigned_64 (Integral_Seconds)) * 10_000_000 +
        Interfaces.Unsigned_64 (Floor ((Seconds_From_Epoch - Duration (Integral_Seconds)) * 10_000_000));
   end Ada_To_Windows_Filetime_Leap;

   Unix_Time_Day : constant := 24 * 60 * 60;

   ------------------------------
   -- Unix_Time_To_Ada_No_Leap --
   ------------------------------

   function Unix_Time_To_Ada_No_Leap (Item : Interfaces.Integer_64)
     return Ada.Calendar.Time
   is
      Moment_In_Day_64 : constant Interfaces.Integer_64 :=
        Item mod Unix_Time_Day;
      Days : constant Ada.Calendar.Arithmetic.Day_Count := Ada.Calendar.Arithmetic.Day_Count
        ((Item - Moment_In_Day_64) / Unix_Time_Day);
      Moment_In_Day : constant Duration :=
        Interfaces.Integer_64'Pos (Moment_In_Day_64) * Duration'(1.0);
   begin
      return Unix_Epoch_Plus (Days) + Moment_In_Day;
   end Unix_Time_To_Ada_No_Leap;

   ---------------------------
   -- Unix_Time_To_Ada_Leap --
   ---------------------------

   function Unix_Time_To_Ada_Leap (Item : Interfaces.Integer_64)
     return Ada.Calendar.Time
   is
      Item_From_Epoch : constant Duration :=
        Interfaces.Integer_64'Pos (Item) * Duration'(1.0);
   begin
      return Unix_Epoch + Item_From_Epoch;
   end Unix_Time_To_Ada_Leap;

   ------------------------------
   -- Ada_To_Unix_Time_No_Leap --
   ------------------------------
   
   function Ada_To_Unix_Time_No_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Integer_64
   is
      Days : Ada.Calendar.Arithmetic.Day_Count;
      Seconds : Duration;
      Integral_Seconds : Natural; -- divide into parts to avoid overflows
      Leap_Seconds : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
   begin
      Difference (Item, Unix_Epoch, Days, Seconds, Leap_Seconds);
      Integral_Seconds := Natural_Floor (Seconds);
      return
        Ada.Calendar.Arithmetic.Day_Count'Pos (Days) * Interfaces.Integer_64'(Unix_Time_Day) +
        Interfaces.Integer_64 (Integral_Seconds);
   end Ada_To_Unix_Time_No_Leap;

   ---------------------------
   -- Ada_To_Unix_Time_Leap --
   ---------------------------
   
   function Ada_To_Unix_Time_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Integer_64
   is
      Seconds_From_Epoch : constant Duration := Item - Unix_Epoch;
   begin
      return Floor (Seconds_From_Epoch);
   end Ada_To_Unix_Time_Leap;

   Javascript_Time_Day : constant := 24 * 60 * 60 * 1000;

   ------------------------------------
   -- Javascript_Time_To_Ada_No_Leap --
   ------------------------------------

   function Javascript_Time_To_Ada_No_Leap (Item : Interfaces.Integer_64)
     return Ada.Calendar.Time
   is
      Moment_In_Day_64 : constant Interfaces.Integer_64 :=
        Item mod Javascript_Time_Day;
      Days : constant Ada.Calendar.Arithmetic.Day_Count := Ada.Calendar.Arithmetic.Day_Count
        ((Item - Moment_In_Day_64) / Javascript_Time_Day);
      Moment_In_Day : constant Duration :=
        Interfaces.Integer_64'Pos (Moment_In_Day_64) * Duration'(1.0);
   begin
      return Unix_Epoch_Plus (Days) + Moment_In_Day;
   end Javascript_Time_To_Ada_No_Leap;

   ---------------------------------
   -- Javascript_Time_To_Ada_Leap --
   ---------------------------------

   function Javascript_Time_To_Ada_Leap (Item : Interfaces.Integer_64)
     return Ada.Calendar.Time
   is
      Item_From_Epoch : constant Duration :=
        Interfaces.Integer_64'Pos (Item) * Duration'(0.001);
   begin
      return Unix_Epoch + Item_From_Epoch;
   end Javascript_Time_To_Ada_Leap;

   ------------------------------------
   -- Ada_To_Javascript_Time_No_Leap --
   ------------------------------------
   
   function Ada_To_Javascript_Time_No_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Integer_64
   is
      Days : Ada.Calendar.Arithmetic.Day_Count;
      Seconds : Duration;
      Integral_Seconds : Natural; -- divide into parts to avoid overflows
      Leap_Seconds : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
   begin
      Difference (Item, Unix_Epoch, Days, Seconds, Leap_Seconds);
      Integral_Seconds := Natural_Floor (Seconds);
      return
        Ada.Calendar.Arithmetic.Day_Count'Pos (Days) * Interfaces.Integer_64'(Javascript_Time_Day) +
        Interfaces.Integer_64 (Integral_Seconds) * 1_000 +
        Floor ((Seconds - Duration (Integral_Seconds)) * 1_000);
   end Ada_To_Javascript_Time_No_Leap;

   ---------------------------------
   -- Ada_To_Javascript_Time_Leap --
   ---------------------------------
   
   function Ada_To_Javascript_Time_Leap (Item : Ada.Calendar.Time)
     return Interfaces.Integer_64
   is
      Seconds_From_Epoch : constant Duration := Item - Unix_Epoch;
      Integral_Seconds : constant Interfaces.Integer_64 := Floor (Seconds_From_Epoch);
   begin
      return
        Integral_Seconds * 1_000 +
        Floor ((Seconds_From_Epoch - Duration (Integral_Seconds)) * 1_000);
   end Ada_To_Javascript_Time_Leap;

   -------------------------------
   -- Yaml_Value_With_Time_Zone --
   -------------------------------

   function Yaml_Value_With_Time_Zone (Date : String; Time_Zone : in out Ada.Calendar.Time_Zones.Time_Offset) return Ada.Calendar.Time is
      -- Ada port of https://bitbucket.org/OCTAGRAM/delphi-yaml/
      Year : Natural := 0;
      Month : Natural := 0;
      Day : Natural := 0;
      Hour : Natural := 0;
      Minute : Natural := 0;
      Second : Natural := 0;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration := 0.0;

      function Build return Ada.Calendar.Time is
        (Ada.Calendar.Formatting.Time_Of
           (Year => Year, Month => Month, Day => Day,
            Hour => Hour, Minute => Minute,
            Second => (if Second = 60 then 59 else Second),
            Sub_Second => Sub_Second, Leap_Second => Second = 60,
            Time_Zone => Time_Zone));

      I : Integer;
      C, D, F : Character;
   begin
      if Date'Length = 0 then
         raise Constraint_Error with "Empty Date string cannot be parsed to Ada.Calendar.Time";
      end if;

      if Date'Length < 14 and Date'Length /= 10 then
         raise Constraint_Error with "Date string length must be either 10 or not less than 14 characters";
      end if;

      if Date (Date'First) not in '0' .. '9' or
        Date (Date'First + 1) not in '0' .. '9' or
        Date (Date'First + 2) not in '0' .. '9' or
        Date (Date'First + 3) not in '0' .. '9' or
        Date (Date'First + 4) /= '-' or
        Date (Date'First + 5) not in '0' .. '9'
      then
         raise Constraint_Error with "Date string does not start with yyyy-m format";
      end if;

      Year :=
        (Character'Pos (Date (Date'First)) - 16#30#) * 1000 +
        (Character'Pos (Date (Date'First + 1)) - 16#30#) * 100 +
        (Character'Pos (Date (Date'First + 2)) - 16#30#) * 10 +
        (Character'Pos (Date (Date'First + 3)) - 16#30#);

      -- ymd
      if Date'Length = 10 and then
        (Date (Date'First + 6) in '0' .. '9' and
         Date (Date'First + 7) = '-' and
         Date (Date'First + 8) in '0' .. '9' and
         Date (Date'First + 9) in '0' .. '9')
      then
         Month :=
           (Character'Pos (Date (Date'First + 5)) - 16#30#) * 10 +
           (Character'Pos (Date (Date'First + 6)) - 16#30#);
         Day :=
           (Character'Pos (Date (Date'First + 8)) - 16#30#) * 10 +
           (Character'Pos (Date (Date'First + 9)) - 16#30#);

         return Build;
      end if;

      -- ymdhmsfz
      if Date'Length < 14 then
         raise Constraint_Error with "Failed to parse Date in yyyy-mm-dd format, but other possible representation requres no less than 14 characters";
      end if;

      C := Date (Date'First + 6);
      case C is
         when '0' .. '9' =>
            if Date (Date'First + 7) /= '-' then
               raise Constraint_Error with "Unexpected character after month";
            end if;
            Month :=
              (Character'Pos (Date (Date'First + 5)) - 16#30#) * 10 +
              (Character'Pos (Date (Date'First + 6)) - 16#30#);
            I := Date'First + 7;
         when '-' =>
            Month := Character'Pos (Date (Date'First + 5)) - 16#30#;
            I := Date'First + 6;
         when others =>
            raise Constraint_Error with "Unexpected character after month";
      end case;

      C := Date (I + 1);
      if C not in '0' .. '9' then
         raise Constraint_Error with "Unexpected character on place of day";
      end if;

      D := Date (I + 2);
      case D is
         when '0' .. '9' =>
            Day := (Character'Pos (C) - 16#30#) * 10 + (Character'Pos (D) - 16#30#);
            case Date (I + 3) is
               when 'T' | 't' =>
                  I := I + 4;
                  C := Date (I);
               when Character'Val (32) | Character'Val (9) =>
                  I := I + 3;
                  loop
                     I := I + 1;
                     if I > Date'Last then
                        raise Constraint_Error with "Unexpected end of string after yyyy-mm-dd whitespace";
                     end if;

                     C := Date (I);
                     case C is
                        when Character'Val (32) | Character'Val (9) =>
                           null;
                        when '0' .. '9' =>
                           exit;
                        when others =>
                           raise Constraint_Error with "Unexpected character after yyyy-mm-dd whitespace";
                     end case;
                  end loop;
               when others =>
                  raise Constraint_Error with "Unexpected character after yyyy-mm-dd";
            end case;
         when 'T' | 't' =>
            I := I + 3;
            C := Date (I);
         when Character'Val (32) | Character'Val (9) =>
            I := I + 2;
            loop
               I := I + 1;
               if I > Date'Last then
                  raise Constraint_Error with "Unexpected end of string after yyyy-mm-dd whitespace";
               end if;

               C := Date (I);
               case C is
                  when Character'Val (32) | Character'Val (9) =>
                     null;
                  when '0' .. '9' =>
                     exit;
                  when others =>
                     raise Constraint_Error with "Unexpected character after yyyy-mm-dd whitespace";
               end case;
            end loop;
         when others =>
            raise Constraint_Error with "Unexpected character after yyyy-mm-dd";
      end case;

      case C is
         when '0' .. '9' =>
            Hour := Character'Pos (C) - 16#30#;
         when others =>
            raise Constraint_Error with "Unexpected character on place of hour";
      end case;

      if I + 2 > Date'Last then
         raise Constraint_Error with "Unexpected end of string near the hour";
      end if;

      C := Date (I + 1);
      case C is
         when '0' .. '9' =>
            if Date (I + 2) /= ':' then
               raise Constraint_Error with "Unexpected character after the hour";
            end if;

            Hour := Hour * 10 + (Character'Pos (C) - 16#30#);
            I := I + 3;
         when ':' =>
            I := I + 2;
         when others =>
            raise Constraint_Error with "Unexpected character after the hour";
      end case;

      if I + 2 > Date'Last then
         raise Constraint_Error with "Unexpected end of string near the minute";
      end if;

      C := Date (I);
      case C is
         when '0' .. '9' =>
            Minute := Character'Pos (C) - 16#30#;
         when others =>
            raise Constraint_Error with "Unexpected character on place of minute";
      end case;

      C := Date (I + 1);
      case C is
         when '0' .. '9' =>
            if Date (I + 2) /= ':' then
               raise Constraint_Error with "Unexpected character after the minute";
            end if;
            Minute := Minute * 10 + (Character'Pos (C) - 16#30#);
            I := I + 3;
         when ':' =>
            I := I + 2;
         when others =>
            raise Constraint_Error with "Unexpected character after the minute";
      end case;

      if I > Date'Last then
         raise Constraint_Error with "Unexpected end of string near the second";
      end if;

      C := Date (I);
      case C is
         when '0' .. '9' =>
            Second := Character'Pos (C) - 16#30#;
         when others =>
            raise Constraint_Error with "Unexpected character on place of second";
      end case;

      if I + 1 <= Date'Last then
         C := Date (I + 1);
         case C is
            when '0' .. '9' =>
               I := I + 2;
               Second := Second * 10 + (Character'Pos (C) - 16#30#);
            when '.' | Character'Val (32) | Character'Val (9) | 'Z' | '+' | '-' =>
               I := I + 1;
            when others =>
               raise Constraint_Error with "Unexpected character after the second";
         end case;
      else
         I := I + 1;
      end if;

      if I <= Date'Last and then Date (I) = '.' then
         declare
            J : Natural := I + 1;
         begin
            while J <= Date'Last and then Date (J) in '0' .. '9' loop
               J := J + 1;
            end loop;

            if J > I + 1 then
               Sub_Second := Ada.Calendar.Formatting.Second_Duration'Value ("0." & Date (I + 1 .. J - 1));
            end if;

            I := J;
         end;
      end if;

      while I <= Date'Last loop
         case Date (I) is
            when Character'Val (32) | Character'Val (9) =>
               I := I + 1;
            when 'Z' | '+' | '-' =>
               exit;
            when others =>
               raise Constraint_Error with "Unexpected character after the second";
         end case;
      end loop;

      if I <= Date'Last then
         F := Date (I);
         case F is
            when 'Z' =>
               Time_Zone := 0; -- override default nonzero value if any
               if I /= Date'Last then
                  raise Constraint_Error with "Unexpected character after the time zone";
               end if;
            when '+' | '-' =>
               if I + 1 > Date'Last then
                  raise Constraint_Error with "Unexpected end of string near the time zone";
               end if;

               C := Date (I + 1);
               case C is
                  when '0' .. '9' =>
                     if I + 2 <= Date'Last then
                        D := Date (I + 2);
                        case D is
                           when '0' .. '9' =>
                              Time_Zone := (Character'Pos (C) - 16#30#) * 600 + (Character'Pos (D) - 16#30#) * 60;
                              I := I + 3;
                           when ':' =>
                              Time_Zone := (Character'Pos (C) - 16#30#) * 60;
                              I := I + 2;
                           when others =>
                              raise Constraint_Error with "Unexpected character after the time zone";
                        end case;
                     else
                        Time_Zone := (Character'Pos (C) - 16#30#) * 60;
                        I := I + 2;
                     end if;
                  when others =>
                     raise Constraint_Error with "Unexpected character after the time zone";
               end case;

               if I <= Date'Last then
                  if Date (I) /= ':' then
                     raise Constraint_Error with "Unexpected character after the time zone";
                  end if;

                  if I + 1 > Date'Last then
                     raise Constraint_Error with "Unexpected end of string near the time zone";
                  end if;

                  C := Date (I + 1);
                  case C is
                     when '0' .. '9' =>
                        if I + 2 <= Date'Last then
                           if I + 2 /= Date'Last then
                              raise Constraint_Error with "Unexpected character after the time zone";
                           end if;

                           D := Date (I + 2);
                           case D is
                              when '0' .. '9' =>
                                 Time_Zone := Time_Zone + (Character'Pos (C) - 16#30#) * 10 + (Character'Pos (D) - 16#30#);
                              when others =>
                                 raise Constraint_Error with "Unexpected character after the time zone";
                           end case;
                        else
                           Time_Zone := Time_Zone + (Character'Pos (C) - 16#30#);
                        end if;
                     when others =>
                        raise Constraint_Error with "Unexpected character after the time zone";
                  end case;
               end if;

               if F = '-' then
                  Time_Zone := -Time_Zone;
               end if;
            when others =>
               raise Constraint_Error with "Unexpected character after the second";
         end case;
      end if;

      return Build;
   end Yaml_Value_With_Time_Zone;

   ----------------
   -- Yaml_Value --
   ----------------

   function Yaml_Value (Date : String; Default_Time_Zone : Ada.Calendar.Time_Zones.Time_Offset := 0) return Ada.Calendar.Time is
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset := Default_Time_Zone;
   begin
      return Yaml_Value_With_Time_Zone (Date, Time_Zone);
   end Yaml_Value;

   --------------------
   -- Zero_Pad_Image --
   --------------------

   function Zero_Pad_Image (Item : Natural; Desired_Length : Natural) return String is
      Temp : constant String := Natural'Image (Item);
      Temp_Trimmed : String renames Temp (Temp'First + 1 .. Temp'Last);
   begin
      if Temp_Trimmed'Length >= Desired_Length then
         return Temp_Trimmed (Temp_Trimmed'Last - (Desired_Length - 1) .. Temp_Trimmed'Last);
      else
         return Ada.Strings.Fixed."*" (Desired_Length - Temp_Trimmed'Length, '0') & Temp_Trimmed;
      end if;
   end Zero_Pad_Image;

   ----------------------
   -- Sub_Second_Image --
   ----------------------

   function Sub_Second_Image (Item : Ada.Calendar.Formatting.Second_Duration) return String is
      Temp : constant String := Ada.Calendar.Formatting.Second_Duration'Image (Item);
      First : constant Natural := Temp'First + 2;
      Last : Natural := Temp'Last;
   begin
      while Last > First and then Temp (Last) = '0' loop
         Last := Last - 1;
      end loop;
      if Last = First then
         return "";  -- " 0.000000000" => ".000000000" => "." => ""
      else
         return Temp (First .. Last);
      end if;
   end Sub_Second_Image;

   ----------------
   -- Yaml_Image --
   ----------------

   function Yaml_Image
     (Date : Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset := 0;
      ISO_Format : Boolean := False;
      Include_Time_Zone : Boolean := False;
      Include_Time : Boolean := True;
      Include_Time_Fraction : Boolean := True)
      return String
   is
      Abs_Time_Zone : constant Natural := abs Natural (Time_Zone);
      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;
      Leap_Second : Boolean;
   begin
      Ada.Calendar.Formatting.Split
        (Date => Date,
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Time_Zone => Time_Zone);

      return Zero_Pad_Image (Year, 4) & "-" & Zero_Pad_Image (Month, 2) & "-" & Zero_Pad_Image (Day, 2) &
        (if not Include_Time then
            ""
         else
            (if ISO_Format then
                "T"
             else
                " ") &
            Zero_Pad_Image (Hour, 2) & ":" & Zero_Pad_Image (Minute, 2) & ":" & Zero_Pad_Image ((if Leap_Second then 60 else Second), 2) &
            (if Include_Time_Fraction then
                Sub_Second_Image (Sub_Second)
             else
                "") &
            (if not Include_Time_Zone then
                ""
             else
                (if ISO_Format then
                    ""
                 else
                    " ") &
                (if ISO_Format and Time_Zone = 0 then
                    "Z"
                 else
                    (if Time_Zone < 0 then
                        "-"
                     else
                        "+") &
                    Zero_Pad_Image (Abs_Time_Zone / 60, 2) & ":" & Zero_Pad_Image (Abs_Time_Zone mod 60, 2))));
   end Yaml_Image;

end Calendar_Conversions;
