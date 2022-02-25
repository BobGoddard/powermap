--  Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
--  This file is free software: you may copy, redistribute and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 2 of the License, or (at your
--  option) any later version.
--
--  This file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Xstrings is
   dirstr_www   : constant Unbounded_String := To_Unbounded_String ("/srv/www/htdocs/svg/");
   dirstr_usr   : constant Unbounded_String := To_Unbounded_String ("/home/b/svg/");
   ind1 : constant Character := Ada.Characters.Latin_1.HT;
   ind2 : constant String := ind1 & ind1;
   ind3 : constant String := ind1 & ind2;
   ind4 : constant String := ind1 & ind3;
   ind5 : constant String := ind1 & ind4;
   ind6 : constant String := ind1 & ind5;
   ind7 : constant String := ind1 & ind6;
   pt   : constant String := "12pt;";
   s1   :          String :=  Ada.Characters.Latin_1.LF &
            ".tooltip {" & Ada.Characters.Latin_1.LF &
            "font-size: " & pt & Ada.Characters.Latin_1.LF &
            "font-family: Verdana;" & Ada.Characters.Latin_1.LF &
            "}" & Ada.Characters.Latin_1.LF &
            ".tooltip_bg {" & Ada.Characters.Latin_1.LF &
            "fill: #D4AF37;" & Ada.Characters.Latin_1.LF &
            "stroke: black;" & Ada.Characters.Latin_1.LF &
            "stroke-width: 1;" & Ada.Characters.Latin_1.LF &
            "opacity: 1;" & Ada.Characters.Latin_1.LF &
            "}" & Ada.Characters.Latin_1.LF &
            ".minor-axis {" & Ada.Characters.Latin_1.LF &
            "stroke: #5050ff;" & Ada.Characters.Latin_1.LF &
            "stroke-dasharray: 8, 4;" & Ada.Characters.Latin_1.LF &
            "}" & Ada.Characters.Latin_1.LF &
            ".major-axis {" & Ada.Characters.Latin_1.LF &
            "stroke: #a0a0ff;" & Ada.Characters.Latin_1.LF &
            "}"  & Ada.Characters.Latin_1.LF &
            ".def-black {"  & Ada.Characters.Latin_1.LF &
            "fill: black;"  & Ada.Characters.Latin_1.LF &
            "font-family: Verdana;"  & Ada.Characters.Latin_1.LF &
            "font-size: " & pt  & Ada.Characters.Latin_1.LF &
            "stroke: none;"  & Ada.Characters.Latin_1.LF &
            "writing-mode: lr;"  & Ada.Characters.Latin_1.LF &
            "}"  & Ada.Characters.Latin_1.LF &
            ".def-red {"  & Ada.Characters.Latin_1.LF &
            "fill: red;"  & Ada.Characters.Latin_1.LF &
            "font-family: Verdana;"  & Ada.Characters.Latin_1.LF &
            "font-size: " & pt  & Ada.Characters.Latin_1.LF &
            "stroke: none;"  & Ada.Characters.Latin_1.LF &
            "writing-mode: lr;"  & Ada.Characters.Latin_1.LF &
            "}"  & Ada.Characters.Latin_1.LF &
            ".data_red {"  & Ada.Characters.Latin_1.LF &
            "fill: #ff0000;"  & Ada.Characters.Latin_1.LF &
            "rx: 5;"  & Ada.Characters.Latin_1.LF &
            "ry: 5;"  & Ada.Characters.Latin_1.LF &
            "}"  & Ada.Characters.Latin_1.LF &
            ".data_orange {"  & Ada.Characters.Latin_1.LF &
            "fill: #ffa500;"  & Ada.Characters.Latin_1.LF &
            "rx: 5;"  & Ada.Characters.Latin_1.LF &
            "ry: 5;"  & Ada.Characters.Latin_1.LF &
            "}"  & Ada.Characters.Latin_1.LF &
            ".data_green {"  & Ada.Characters.Latin_1.LF &
            "fill: #00ff00;"  & Ada.Characters.Latin_1.LF &
            "rx: 5;"  & Ada.Characters.Latin_1.LF &
            "ry: 5;"  & Ada.Characters.Latin_1.LF &
            "}"  & Ada.Characters.Latin_1.LF &
            "";

   cdata_str : String :=
                 ind3 & "function init(evt, sw, sh) {" & Ada.Characters.Latin_1.LF &
                 ind4 & "if ( window.svgDocument == null ) {" & Ada.Characters.Latin_1.LF &
                 ind5 & "svgDocument = evt.target.ownerDocument;" & Ada.Characters.Latin_1.LF &
                 ind4 & "}" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_kwh        = svgDocument.getElementById('tooltip_kwh');"        & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_kwh   = svgDocument.getElementById('tooltip_cost_kwh');"   & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_net   = svgDocument.getElementById('tooltip_cost_net');"   & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_gross = svgDocument.getElementById('tooltip_cost_gross');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_date       = svgDocument.getElementById('tooltip_date');"       & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_bg         = svgDocument.getElementById('tooltip_bg');"         & Ada.Characters.Latin_1.LF &
--                 ind4 & "tooltip_test       = svgDocument.getElementById('tooltip_test');"     & Ada.Characters.Latin_1.LF &
--                 ind4 & "tooltip_bg1        = svgDocument.getElementById('tooltip_bg1');"     & Ada.Characters.Latin_1.LF &
                 ind4 & "svgwidth           = sw;" & Ada.Characters.Latin_1.LF &
                 ind4 & "svgheight          = sh;" & Ada.Characters.Latin_1.LF &
                 ind3 & "}" & Ada.Characters.Latin_1.LF &

                 ind3 & "function ShowTooltip(evt, ms_kwh, ms_cost_kwh, ms_cost_net, ms_cost_gross, ms_date) {" & Ada.Characters.Latin_1.LF &
                 ind4 & "x = evt.clientX;"                                                  & Ada.Characters.Latin_1.LF &
                 ind4 & "y = evt.clientY;"                                                  & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_kwh       .firstChild.data = ms_kwh;"                      & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_kwh  .firstChild.data = ms_cost_kwh;"                 & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_net  .firstChild.data = ms_cost_net;"                 & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_gross.firstChild.data = ms_cost_gross;"               & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_date      .firstChild.data = ms_date;"                     & Ada.Characters.Latin_1.LF &
                 ind4 & "length = Math.max(tooltip_kwh.getComputedTextLength(), "           &
                                          "tooltip_cost_kwh  .getComputedTextLength (), "   &
                                          "tooltip_cost_net  .getComputedTextLength (), "   &
                                          "tooltip_cost_gross.getComputedTextLength (), "   &
                                          "tooltip_date      .getComputedTextLength ()); "  & Ada.Characters.Latin_1.LF &
--                 ind4 & "bb = tooltip_cost_kwh.getBBox();"              & Ada.Characters.Latin_1.LF &
--                 ind4 & "tooltip_test    .firstChild.data = bb.height;" & Ada.Characters.Latin_1.LF &

                 ind4 & "if (x + length + 8 + 11 > svgwidth) {"                             & Ada.Characters.Latin_1.LF &
                 ind5 & "x = x - length - 5;"                                               & Ada.Characters.Latin_1.LF &
                 ind4 & "}"                                                                 & Ada.Characters.Latin_1.LF &
                 ind4 & "if (ms_date == 'undefined') {"                                     & Ada.Characters.Latin_1.LF &
                 ind5 & "height = 5 + 18 * 4;"                                              & Ada.Characters.Latin_1.LF &
                 ind4 & "} else {"                                                          & Ada.Characters.Latin_1.LF &
                 ind5 & "height = 5 + 18 * 5;"                                              & Ada.Characters.Latin_1.LF &
                 ind4 & "}"                                                                 & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_kwh       .setAttributeNS(null, 'x', x +  14);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_kwh       .setAttributeNS(null, 'y', y +  32);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_kwh  .setAttributeNS(null, 'x', x +  14);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_kwh  .setAttributeNS(null, 'y', y +  50);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_net  .setAttributeNS(null, 'x', x +  14);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_net  .setAttributeNS(null, 'y', y +  68);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_gross.setAttributeNS(null, 'x', x +  14);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_gross.setAttributeNS(null, 'y', y +  86);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_date      .setAttributeNS(null, 'x', x +  14);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_date      .setAttributeNS(null, 'y', y + 104);"            & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_bg        .setAttributeNS(null, 'width', length + 8);"     & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_bg        .setAttributeNS(null, 'height', height);"        & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_bg        .setAttributeNS(null, 'x', x + 10);"             & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_bg        .setAttributeNS(null, 'y', y + 16);"             & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_kwh       .setAttributeNS(null, 'visibility', 'visible');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_kwh  .setAttributeNS(null, 'visibility', 'visible');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_net  .setAttributeNS(null, 'visibility', 'visible');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_gross.setAttributeNS(null, 'visibility', 'visible');" & Ada.Characters.Latin_1.LF &
                 ind4 & "if (ms_date == 'undefined') {"                                     & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_date      .setAttributeNS(null, 'visibility', 'hidden');"  & Ada.Characters.Latin_1.LF &
                 ind4 & "} else {"                                                          & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_date      .setAttributeNS(null, 'visibility', 'visible');" & Ada.Characters.Latin_1.LF &
                 ind4 & "}"                                                                 & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_bg        .setAttributeNS(null, 'visibility', 'visible');" & Ada.Characters.Latin_1.LF &
--                 ind4 & "tooltip_test    .setAttributeNS(null, 'visibility', 'visible');" & Ada.Characters.Latin_1.LF &
--                 ind4 & "tooltip_bg1     .setAttributeNS(null, 'visibility', 'visible');" & Ada.Characters.Latin_1.LF &
                 ind3 & "}" & Ada.Characters.Latin_1.LF &

                 ind3 & "function HideTooltip(evt) {"                                      & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_kwh       .setAttributeNS(null, 'visibility', 'hidden');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_kwh  .setAttributeNS(null, 'visibility', 'hidden');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_net  .setAttributeNS(null, 'visibility', 'hidden');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_cost_gross.setAttributeNS(null, 'visibility', 'hidden');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_date      .setAttributeNS(null, 'visibility', 'hidden');" & Ada.Characters.Latin_1.LF &
                 ind4 & "tooltip_bg        .setAttributeNS(null, 'visibility', 'hidden');" & Ada.Characters.Latin_1.LF &
--                 ind4 & "tooltip_test    .setAttributeNS(null, 'visibility', 'hidden');" & Ada.Characters.Latin_1.LF &
--                 ind4 & "tooltip_bg1     .setAttributeNS(null, 'visibility', 'hidden');" & Ada.Characters.Latin_1.LF &
                 ind3 & "}" & Ada.Characters.Latin_1.LF;

end Xstrings;
