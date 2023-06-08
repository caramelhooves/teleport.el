Feature: List teleport hosts

  Scenario: Happy path
  Given tsh ls prints content of "single_host.ls.json"
  Given I call "teleport-list-nodes"
  Then I should be in buffer "*Teleport Nodes List*"
  And I wait for 3 seconds
  Then I should be in buffer "*Teleport Nodes List*"
  And the buffer should contain content of "single_host.ls.txt"
