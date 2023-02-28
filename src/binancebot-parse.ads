with Ada.Containers.Vectors;
with AWS.Response;

with League.Strings;
with League.JSON.Objects;
with League.JSON.Arrays;

package Binancebot.Parse is

   type Type_Lot_Size is record
      minQty : League.Strings.Universal_String;
      stepSize : League.Strings.Universal_String;
      maxQty : League.Strings.Universal_String;
   end record;

   type Type_TokInfo_Pull is record
      First_Pair : League.Strings.Universal_String;
      First_Price : Long_Float;
      First_Price_Txt : League.Strings.Universal_String;
      First_MinQTY : Long_Float;
      First_MaxQTY : Long_Float;
      First_StepSize : Long_Float;

      Second_Pair : League.Strings.Universal_String;
      Second_Price : Long_Float;
      Second_Price_Txt : League.Strings.Universal_String;
      Second_MinQTY : Long_Float;
      Second_MaxQTY : Long_Float;
      Second_StepSize : Long_Float;

      Third_Pair : League.Strings.Universal_String;
      Third_Price : Long_Float;
      Third_Price_Txt : League.Strings.Universal_String;
      Third_MinQTY : Long_Float;
      Third_MaxQTY : Long_Float;
      Third_StepSize : Long_Float;
   end record;

   package TokInfo_Pull_Vectors is new Ada.Containers.Vectors (Index_Type => Positive,
                                                               Element_Type => Type_TokInfo_Pull);
   subtype TokInfo_Pull_Vector is TokInfo_Pull_Vectors.Vector;
   subtype TokInfo_Pull_Cursor is TokInfo_Pull_Vectors.Cursor;

   type Type_ExchangeInfo is record
      Trading_Symbols : League.JSON.Arrays.JSON_Array;
      Trading_Pull : TokInfo_Pull_Vector;
   end record;

   type Type_TickerPair is record
      Scrip1 : Float;
      Scrip2 : Float;
      Scrip3 : Float;
   end record;

   procedure Get_Crypto_Combinations (Symbols : in League.JSON.Arrays.JSON_Array;
                                      Base : in League.Strings.Universal_String;
                                      Pull : out TokInfo_Pull_Vector);
   function Get_LotSizes_Len(Filters : in League.JSON.Arrays.JSON_Array;
                            Sizes : out Type_Lot_Size) return Boolean;

   function Parse_ExchangeInfo
     (Data : AWS.Response.Data) return Type_ExchangeInfo;

   function Parse_TickerPrice (Data : AWS.Response.Data) return League.Strings.Universal_String;
   function Parse_TickerPricePoll (Data : AWS.Response.Data) return League.JSON.Arrays.JSON_Array;
   procedure Apply_TickerPrices_To_Poll(Prices : in League.JSON.Arrays.JSON_Array;
                                        Poll : out Type_TokInfo_Pull);

end Binancebot.Parse;
