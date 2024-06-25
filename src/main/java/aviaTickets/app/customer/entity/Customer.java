package aviaTickets.app.customer.entity;

import java.time.LocalDateTime;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record Customer(
  @Positive
  Integer id,
  @NotEmpty
  String name,
  @NotEmpty
  String email,
  @NotEmpty
  String password,
  LocalDateTime createdAt,
  LocalDateTime updatedAt,
  Boolean isBanned,
  Role role // false -> is a customer; true -> is an admin; 
) {

  // public User {
  //   if(!completedOn.isAfter(startedOn)) {
  //     throw new IllegalArgumentException("complited on must be after Started on!");
  //   }
  // }
}

