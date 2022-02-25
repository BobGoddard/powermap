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

with Record_Types; use Record_Types;

package Update_Record is
   procedure Update_Record_r       (r : in out powerrec);
   procedure Update_Record_l_r     (l : powerrec; r : in out powerrec);
   procedure Update_Day_Record_r   (r : in out powerrec);
   procedure Update_Day_Record_l_r (l : powerrec; r : in out powerrec);
end Update_Record;
