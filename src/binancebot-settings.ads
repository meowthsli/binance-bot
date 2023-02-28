package Binancebot.Settings is

   type Type_Settings is record
      API_Token : String(1..64);
      Private_Key : String(1..64);
      INVESTMENT_AMOUNT_DOLLARS : Long_Float;
      MIN_PROFIT_DOLLARS : Long_Float;
      BROKERAGE_PER_TRANSACTION_PERCENT : Long_Float;
   end record;

   Global_Config : Binancebot.Settings.Type_Settings;

   procedure Get_Settings_From_File(Settings : out Type_Settings;
                                   Fileconf : String := "config.ini");

end Binancebot.Settings;
