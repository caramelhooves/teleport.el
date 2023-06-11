Feature: List teleport hosts

  Scenario Outline: Happy path
  Given tsh ls prints content of "<json>"
  Given I call "teleport-list-nodes"
  Then I should be in buffer "*Teleport Nodes List*"
  And I wait for 3 seconds
  Then I should be in buffer "*Teleport Nodes List*"
  And the buffer should contain content of "<txt>"

  Examples:
  | json                | txt                |
  | single_host.ls.json | single_host.ls.txt |
  | three_hosts.ls.json | three_hosts.ls.txt |
