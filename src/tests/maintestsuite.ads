with AUnit.Test_Suites;

package MainTestSuite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   procedure runAll;
end MainTestSuite;
