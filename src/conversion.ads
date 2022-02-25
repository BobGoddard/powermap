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

package Conversion is
   function CF_power       (power       : Long_Integer) return Float;
   function CF_kwcostnet   (kwcostnet   : Long_Integer) return Float;
   function CF_sccostnet   (sccostnet   : Long_Integer) return Float;
   function CF_kwcostgross (kwcostgross : Long_Integer) return Float;
   function CF_sccostgross (sccostgross : Long_Integer) return Float;

private
   cf_kwrate    : constant Float := 100000.0;
   cf_scrate    : constant Float := 100000.0;
   cf_vatrate   : constant Float :=  10000.0;
   cf_powerrate : constant Float :=   3200.0;
end Conversion;
