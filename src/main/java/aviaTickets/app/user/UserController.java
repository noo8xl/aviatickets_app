package aviaTickets.app.user;

import java.util.List;
import java.util.Optional;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import aviaTickets.app.exception.NotFoundException;
import aviaTickets.app.user.entity.User;
import jakarta.validation.Valid;

@RestController
@RequestMapping("/api/users")
public class UserController {
  
  private final UserRepository userRepository;

  public UserController(UserRepository userRepository) {
    this.userRepository = userRepository;
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("")
  List<User> findAll() {
    return this.userRepository.getAll();
  }
  
  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get/{email}")
  User findById(@PathVariable String email) {
    Optional<User> user = userRepository.getUser(email);
    if(user.isEmpty()) {
      throw new NotFoundException();
    }
    return user.get();
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get/{id}")
  User findById(@PathVariable Integer id) {
    Optional<User> user = userRepository.getUser(id);
    if(user.isEmpty()) {
      throw new NotFoundException();
    }
    return user.get();
  }

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/create")
  void create(@Valid @RequestBody User user) {
    userRepository.createUser(user.name(), user.password(), user.email());
  }

  @ResponseStatus(HttpStatus.NO_CONTENT)
  @PutMapping("/update/{id}")
  void update(@Valid @RequestBody User user, @PathVariable Integer id) {
    userRepository.updateProfile(user, id);
  }

  @ResponseStatus(HttpStatus.NO_CONTENT)
  @DeleteMapping("/delete/{id}")
  void delete(@PathVariable Integer id) {
    userRepository.deleteUser(id);
  }

}
