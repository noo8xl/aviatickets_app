package aviaTickets.app.user.entity;

import java.time.LocalDateTime;
// import jakarta.validation.constraints.NotEmpty;
// import jakarta.validation.constraints.Positive;

// enum Role {
//   USER,
//   ADMIN
// }

public record User(
  Integer id,
  String name,
  String email,
  String password,
  LocalDateTime createdAt,
  LocalDateTime updatedAt,
  Role role // false -> is a customer; true -> is an admin; 
) {

  // public User {
  //   if(!completedOn.isAfter(startedOn)) {
  //     throw new IllegalArgumentException("complited on must be after Started on!");
  //   }
  // }
}
