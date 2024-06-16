package aviaTickets.app.user;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

import aviaTickets.app.user.entity.User;
import aviaTickets.app.user.entity.Role;


// UserInteraction -> describe the main User interaction logic
abstract class UserInteraction {
  // create user
  abstract void createUser(String name, String password, String email);
  // get user data by id 
  abstract Optional<User> getUser(Integer id);
  // get user data bu email
  abstract Optional<User> getUser(String email);
  // get user list 
  abstract List<User> getAll();
  // update user data by <id> key with dto as second argument 
  abstract void updateProfile(User u, Integer id);
  // delete user by id
  abstract void deleteUser(Integer id);
}

@Repository
public class UserRepository extends UserInteraction {
  
  private static final Logger log = LoggerFactory.getLogger(UserRepository.class);
  private final JdbcClient jdbcClient;

  public UserRepository(JdbcClient jdbcClient) {
    this.jdbcClient = jdbcClient;
  }

  
  public void createUser(String name, String password, String email) {
    User user = new User(
      null, 
      name, 
      email, 
      password, 
      LocalDateTime.now(), 
      LocalDateTime.now(),
      Role.USER
    );

    log.info("user: " + user);
    this.save(user);
  }


  public Optional<User> getUser(Integer id) {
    return this.findById(id);
  }
  
  public Optional<User> getUser(String email) {
    return this.findByEmail(email);
  }
  
  public List<User> getAll() {
    return this.findAll();
  }

  public void updateProfile(User u , Integer id) {
    this.update(u, id);
  }

  public void deleteUser(Integer id) {
    this.delete(id);
  }


  // ### ----------------------------------------------------------------------------------- ###

  private void save(User user) {

    String userBase = "INSERT INTO user (name, email, password) VALUES (?,?,?)"; 
    String userParams = "INSERT INTO user_params (created_at, updated_at, role, user_id) VALUES (?,?,?,?)";
    // base user data 

    var updated = jdbcClient.sql(userBase)
      .params(List.of(user.name(), user.email(), user.password()))
      .update();

    Assert.state(updated == 1, "Failed to create user " + user.name());

    // get saved user 
    Optional<User> savedUser = this.findByEmail(user.email());

    // user params data
    updated += jdbcClient.sql(userParams) 
      .params(List.of(user.createdAt(), user.updatedAt(), user.role(), savedUser.get().id()))
      .update();

    Assert.state(updated == 2, "Failed to create user " + user.name());
  }

  private List<User> findAll() {
    String sqlStr = "SELECT user.id, user.name, user.email, user_details.created_at, user_details.updated_at, user_details.role "
      + "FROM user " 
      + "INNER JOIN user_details ON user.id = user_details.user_id ";

    return jdbcClient.sql(sqlStr)
      .query(User.class)
      .list();
  }


  private Optional<User> findById(Integer id) {

    String sqlStr = "SELECT user.id, user.name, user.email, user_details.created_at, user_details.updated_at, user_details.role "
      + "FROM user " 
      + "INNER JOIN user_details ON user.id = user_details.user_id "
      + "WHERE user.id=?";

    return jdbcClient.sql(sqlStr)
      .param(id)
      .query(User.class)
      .optional();
  }

  
  private Optional<User> findByEmail(String email) {

    String sqlStr = "SELECT user.id, user.name, user.email, user_details.created_at, user_details.updated_at, user_details.role "
      + "FROM user " 
      + "INNER JOIN user_details ON user.id = user_details.user_id "
      + "WHERE user.email=?";

    return jdbcClient.sql(sqlStr)
      .param(email)
      .query(User.class)
      .optional();
  }

  private void update(User u, Integer id) {

  }

  private void delete(Integer id) {

  }

}
