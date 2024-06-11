package entity;

import java.time.LocalDateTime;

import service.DatabaseService;
import service.EmailService;

// CustomerInteraction -> describe the main User class logic
abstract class CustomerInteraction {
  // set user data 
  abstract void setUser(String email);
  // get user data by id 
  abstract User getUser(Long id);
  // get user data bu email
  abstract User getUser(String email);
  // update user data by <id> key with dto as second argument 
  abstract User updateProfile(Long id);
}

// User -> interacts with customer CRUD operations
public class User extends CustomerInteraction {

  private Long id;
  private String userName;
  private String userEmail;
  private LocalDateTime createdAt;
  private LocalDateTime updatedAt;

  // inject db service to interact with 
  private DatabaseService dbService  = new DatabaseService();
  // inject email service to send notifications
  private EmailService emailService = new EmailService();


  public User(){};

  public void setUser(String email) { // < ---- should be updated 
    // set user by received dto object
  }

  public User getUser(String email) {

    return this;
  }
  
  public User getUser(Long id) {
    return this;
  }

  public User updateProfile(Long id) {
    // call db for the data updating 
    // and return updated user object
    return this;
  }

  // ### ------------------------------------------------- ###
  // ### --------------- private area below -------------- ###
  // ### ------------------------------------------------- ###

  private void doSome(){}

}
