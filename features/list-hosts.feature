Feature: List teleport nodes

  Scenario Outline: Happy path
  Given tsh ls prints content of "<json>"
  Given I call "teleport-list-nodes"
  Then I should be in buffer "*Teleport Nodes List*"
  And I wait for 3 seconds
  Then I should be in buffer "*Teleport Nodes List*"
  And the buffer should contain content of "<txt>"

  Examples:
  | json                | txt                |
  | single_node.ls.json | single_node.ls.txt |
  | three_nodes.ls.json | three_nodes.ls.txt |
