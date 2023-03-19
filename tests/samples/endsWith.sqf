params ["_string", "_pattern"];
_string select [count _string - count _pattern] == _pattern
