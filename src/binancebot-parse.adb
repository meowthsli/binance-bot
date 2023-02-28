with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Characters.Conversions;

with League.Strings;
with League.JSON.Documents;
with League.JSON.Values;
with League.Holders;

with Binancebot.Util;
with Binancebot.Triangular;
with Binancebot.Settings;

package body Binancebot.Parse is

   use Binancebot.Util;
   use League.Strings;

   procedure Get_Crypto_Combinations (Symbols : in League.JSON.Arrays.JSON_Array;
                                      Base : in League.Strings.Universal_String;
                                      Pull : out TokInfo_Pull_Vector) is
      Tmp_Counter  : Integer;
      Tmp_Counter2 : Integer;
      Tmp_Counter3 : Integer;

      Sym1_Token1 : Universal_String;
      Sym1_Token2 : Universal_String;
      Sym2_Token1 : Universal_String;
      Sym2_Token2 : Universal_String;
      Sym3_Token1 : Universal_String;
      Sym3_Token2 : Universal_String;

      procedure Find_Filters (Symbols : in League.JSON.Arrays.JSON_Array;
                              Symbol : in League.Strings.Universal_String;
                              Lot_Sizes : out Type_Lot_Size) is
         Counter : Positive;
         Result : Boolean;
      begin
         for Counter in 1 .. Symbols.Length loop
            if Symbols (Counter).To_Object.Value (+"symbol").To_String = Symbol then

               Result := Get_LotSizes_Len(Filters => Symbols (Counter).To_Object.Value (+"filters").To_Array,
                                          Sizes => Lot_Sizes);
            end if;
         end loop;
      end Find_Filters;
   begin
      for Tmp_Counter in 1 .. Symbols.Length loop
         Sym1_Token1 := Symbols (Tmp_Counter).To_Object.Value (+"baseAsset").To_String;
         Sym1_Token2 := Symbols (Tmp_Counter).To_Object.Value (+"quoteAsset").To_String;

         if Sym1_Token2 = Base then
            for Tmp_Counter2 in 1 .. Symbols.Length loop
               Sym2_Token1 := Symbols (Tmp_Counter2).To_Object.Value (+"baseAsset").To_String;
               Sym2_Token2 := Symbols (Tmp_Counter2).To_Object.Value (+"quoteAsset").To_String;

               if Sym1_Token1 = Sym2_Token2 then
                  for Tmp_Counter3 in 1 .. Symbols.Length loop
                     Sym3_Token1 := Symbols (Tmp_Counter3).To_Object.Value (+"baseAsset").To_String;
                     Sym3_Token2 := Symbols (Tmp_Counter3).To_Object.Value (+"quoteAsset").To_String;

                     if (Sym2_Token1 = Sym3_Token1) and (Sym3_Token2 = Sym1_Token2) then
                        if (Sym1_Token1 /= Sym1_Token2) and (Sym2_Token1 /= Sym1_Token1) and (Sym2_Token1 /= Sym1_Token2) then
                           declare
                              TMP : Universal_String := +"Base: " & Sym1_Token2 & League.Strings.To_Universal_String (" Intermediate: ") & Sym1_Token1 & League.Strings.To_Universal_String (" Ticker: ") & Sym2_Token1;

                              S1 : Universal_String := Sym1_Token1 & Sym1_Token2;
                              S2 : Universal_String := Sym2_Token1 & Sym1_Token1;
                              S3 : Universal_String := Sym2_Token1 & Sym1_Token2;

                              Item_Pull : Type_TokInfo_Pull;
                              Lot_Sizes : Type_Lot_Size;

                              use Ada.Characters.Conversions;
                           begin

                              Find_Filters(Symbols, S1, Lot_Sizes);

                              Item_Pull.First_Pair := S1;
                              Item_Pull.First_StepSize := Binancebot.Util.U2F(Lot_Sizes.StepSize);
                              Item_Pull.First_minQty := Binancebot.Util.U2F(Lot_Sizes.minQty);
                              Item_Pull.First_maxQty := Binancebot.Util.U2F(Lot_Sizes.maxQty);

                              Find_Filters(Symbols, S2, Lot_Sizes);

                              Item_Pull.Second_Pair := S2;
                              Item_Pull.Second_StepSize := Binancebot.Util.U2F(Lot_Sizes.StepSize);
                              Item_Pull.Second_minQty := Binancebot.Util.U2F(Lot_Sizes.minQty);
                              Item_Pull.Second_maxQty := Binancebot.Util.U2F(Lot_Sizes.maxQty);

                              Find_Filters(Symbols, S3, Lot_Sizes);

                              Item_Pull.Third_Pair := S3;
                              Item_Pull.Third_StepSize := Binancebot.Util.U2F(Lot_Sizes.StepSize);
                              Item_Pull.Third_minQty := Binancebot.Util.U2F(Lot_Sizes.minQty);
                              Item_Pull.Third_maxQty := Binancebot.Util.U2F(Lot_Sizes.maxQty);

                              Pull.Append(Item_Pull);
                           end;
                        end if;
                     end if;
                  end loop;
               end if;
            end loop;
         end if;
      end loop;
   end Get_Crypto_Combinations;

   function Get_LotSizes_Len(Filters : in League.JSON.Arrays.JSON_Array;
                             Sizes : out Type_Lot_Size) return Boolean is
      use League.JSON.Objects;
      use League.Strings;

      Tmp : JSON_Object;
      Tmp_Counter : Natural;
   begin
      for Tmp_Counter in 1 .. Filters.Length loop
         Tmp := Filters (Tmp_Counter).To_Object;
         if Tmp.Value(+"filterType").To_String = +"LOT_SIZE" then

            Sizes.minQty := Tmp.Value(+"minQty").To_String;
            Sizes.maxQty := Tmp.Value(+"maxQty").To_String;
            Sizes.stepSize := Tmp.Value(+"stepSize").To_String;

            return true;
         end if;
      end loop;
      return false;
   end Get_LotSizes_Len;

   function Parse_ExchangeInfo (Data : AWS.Response.Data) return Type_ExchangeInfo
   is
      Document     : League.JSON.Documents.JSON_Document;
      JObject      : League.JSON.Objects.JSON_Object;
      ExchangeInfo : Type_ExchangeInfo;

      Tmp_Counter : Integer;
      Symbols : League.JSON.Arrays.JSON_Array;
   begin
      Document := League.JSON.Documents.From_JSON (AWS.Response.Message_Body (Data));
      JObject := Document.To_JSON_Object;

      -- The symbols processing
      Symbols := JObject.Value (+"symbols").To_Array;


      -- Filtering for only Trading tokens
      for Tmp_Counter in 1 .. Symbols.Length loop
         declare
            Tmp_Obj : League.JSON.Objects.JSON_Object := Symbols (Tmp_Counter).To_Object;
         begin
            if Tmp_Obj.Value (+"status").To_String = +"TRADING" then
               ExchangeInfo.Trading_Symbols.Append (Tmp_Obj.To_JSON_Value);
            end if;
         end;
      end loop;

      Get_Crypto_Combinations (ExchangeInfo.Trading_Symbols, +"USDT", ExchangeInfo.Trading_Pull);

      return ExchangeInfo;
end Parse_ExchangeInfo;

   function Parse_TickerPrice (Data : AWS.Response.Data) return League.Strings.Universal_String is
      Document : League.JSON.Documents.JSON_Document := League.JSON.Documents.From_JSON (AWS.Response.Message_Body (Data));
      JObject : League.JSON.Objects.JSON_Object := Document.To_JSON_Object;
      TPrice : League.Strings.Universal_String := JObject.Value (+"price").To_String;
   begin
      return TPrice;
   end Parse_TickerPrice;

   function Parse_TickerPricePoll (Data : AWS.Response.Data) return League.JSON.Arrays.JSON_Array is
      Document : League.JSON.Documents.JSON_Document := League.JSON.Documents.From_JSON (AWS.Response.Message_Body (Data));
      JArr : League.JSON.Arrays.JSON_Array := Document.To_JSON_Array;
   begin
     return JArr;
   end Parse_TickerPricePoll;

   procedure Apply_TickerPrices_To_Poll(Prices : in League.JSON.Arrays.JSON_Array;
                                        Poll : out Type_TokInfo_Pull) is
      Counter : Positive := 1;
      Object : League.JSON.Objects.JSON_Object;

      use Binancebot.Util;
   begin

      for Counter in 1 .. Prices.Length loop

         Object := Prices(Counter).To_Object;
         if Object.Value(+"symbol").To_String = Poll.First_Pair then
            Poll.First_Price := Binancebot.Util.U2F(Object.Value(+"price").To_String);
            Poll.First_Price_Txt := Object.Value(+"price").To_String;
         elsif Object.Value(+"symbol").To_String = Poll.Second_Pair then
            Poll.Second_Price := Binancebot.Util.U2F(Object.Value(+"price").To_String);
            Poll.Second_Price_Txt := Object.Value(+"price").To_String;
         elsif Object.Value(+"symbol").To_String = Poll.Third_Pair then
            Poll.Third_Price := Binancebot.Util.U2F(Object.Value(+"price").To_String);
            Poll.Third_Price_Txt := Object.Value(+"price").To_String;
         end if;
      end loop;
   end Apply_TickerPrices_To_Poll;
end Binancebot.Parse;
