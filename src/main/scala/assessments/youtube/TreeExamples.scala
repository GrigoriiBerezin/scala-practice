package assessments.youtube

import assessments.youtube.TreeNode.{Empty, Leaf}

object TreeExamples {
  val exampleTree: Leaf[Int] = Leaf(-10,
    Leaf(9, Empty, Empty),
    Leaf(20,
      Leaf(-3,
        Leaf(5,
          Leaf(-2,
            Empty,
            Empty
          ),
          Empty
        ),
        Leaf(-4,
          Empty,
          Empty
        )
      ),
      Leaf(8,
        Empty,
        Leaf(-2,
          Empty,
          Empty
        )
      )
    )
  )
}
