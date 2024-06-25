package aviaTickets.app.customer.dto;

import jakarta.validation.constraints.NotEmpty;

public record ChangePwdDto(
  @NotEmpty
  String email,
  @NotEmpty
  String pwd
) {}
