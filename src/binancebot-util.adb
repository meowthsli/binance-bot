package body Binancebot.Util is

   function "+" (Text : Wide_Wide_String)
                    return League.Strings.Universal_String
                    renames League.Strings.To_Universal_String;

end Binancebot.Util;
