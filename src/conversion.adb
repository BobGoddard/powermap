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

package body Conversion is
--      power_used       : Long_Integer := 0;
--      kwcostnet        : Long_Integer := 0;
--      sccostnet        : Long_Integer := 0; -- sc
--      kwcostgross      : Long_Integer := 0; --    * VAT
--      sccostgross      : Long_Integer := 0; -- sc * VAT
--   10000.0 / 100000.0 11.630 11630 0.11630

   function CF_kwcostgross (kwcostgross : Long_Integer) return Float is
   begin
      return Float (kwcostgross) / cf_kwrate / cf_powerrate / cf_vatrate;
   end CF_kwcostgross;

   function CF_kwcostnet   (kwcostnet : Long_Integer) return Float is
   begin
      return Float (kwcostnet) / cf_kwrate / cf_powerrate;
   end CF_kwcostnet;

   function CF_power       (power : Long_Integer) return Float is
   begin
      return Float (power) / cf_powerrate;
   end CF_power;

   function CF_sccostgross (sccostgross : Long_Integer) return Float is
   begin
      return Float (sccostgross) / cf_scrate / cf_vatrate;
   end CF_sccostgross;

   function CF_sccostnet   (sccostnet : Long_Integer) return Float is
   begin
      return Float (sccostnet) / cf_scrate;
   end CF_sccostnet;
end Conversion;
