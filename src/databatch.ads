with Ada.Containers.Vectors; use Ada.Containers;

with MathUtils;

package DataBatch is
   pragma Assertion_Policy (Pre => Check,
                            Post => Check,
                            Type_Invariant => Check);

   package VecPkg is new Ada.Containers.Vectors(Index_Type   => Positive,
                                                Element_Type => MathUtils.Vector,
                                                "="          => MathUtils.Float_Vec."=");
   type Batch is tagged limited record
      data: VecPkg.Vector;
   end record;

   function size(b: in Batch) return Natural;
   procedure reserve(b: in out Batch; count: Positive);
   procedure randomize(b: in out Batch);
   procedure append(b: in out Batch; vec: MathUtils.Vector);

   function contains(b: in Batch; vec: in MathUtils.Vector) return Boolean;

end DataBatch;
