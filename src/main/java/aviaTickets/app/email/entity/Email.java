package aviaTickets.app.email.entity;

import jakarta.validation.constraints.NotEmpty;

public record Email(
  @NotEmpty
  String userEmail,
  @NotEmpty
  String activationLink,
  @NotEmpty
  String content,
  @NotEmpty
  String date // -> update type (should display smt like "2020/10/06 12:30:55") 
) {} 