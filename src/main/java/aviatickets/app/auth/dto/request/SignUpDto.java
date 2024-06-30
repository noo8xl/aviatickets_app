package aviatickets.app.auth.dto.request;

import jakarta.validation.constraints.NotEmpty;

public record SignUpDto(
  @NotEmpty
  String name,
  @NotEmpty
  String email,
  @NotEmpty
  String password
) {}
