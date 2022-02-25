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

package body Update_Record is

   procedure Update_Day_Record_l_r (l : powerrec; r : in out powerrec) is
   begin
      r.totalpower         := l.totalpower         + r.power_used;
      r.totalkwcostnet     := l.totalkwcostnet     + r.kwcostnet;
      r.totalkwcostgross   := l.totalkwcostgross   + r.kwcostgross;
      r.totalsccostnet     :=                        r.sccostnet;
      r.totalsccostgross   :=                        r.sccostgross;
   end Update_Day_Record_l_r;

   procedure Update_Day_Record_r (r : in out powerrec) is
   begin
      r.totalpower         := r.power_used;
      r.totalkwcostnet     := r.kwcostnet;
      r.totalkwcostgross   := r.kwcostgross;
      r.totalsccostnet     := r.sccostnet;
      r.totalsccostgross   := r.sccostgross;
   end Update_Day_Record_r;

   procedure Update_Record_l_r (l : powerrec; r : in out powerrec) is
   begin
      r.totalpower         := l.totalpower         + r.power_used;
      r.totalkwcostnet     := l.totalkwcostnet     + r.kwcostnet;
      r.totalkwcostgross   := l.totalkwcostgross   + r.kwcostgross;
      r.totalsccostnet     := l.totalsccostnet     + r.sccostnet;
      r.totalsccostgross   := l.totalsccostgross   + r.sccostgross;
   end Update_Record_l_r;

   procedure Update_Record_r (r : in out powerrec) is
   begin
      r.totalpower         := r.power_used;
      r.totalkwcostnet     := r.kwcostnet;
      r.totalkwcostgross   := r.kwcostgross;
      r.totalsccostnet     := r.sccostnet;
      r.totalsccostgross   := r.sccostgross;
   end Update_Record_r;
end Update_Record;
