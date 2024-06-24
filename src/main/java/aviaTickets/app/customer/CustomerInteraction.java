package aviaTickets.app.customer;

import java.util.List;
import java.util.Optional;

import aviaTickets.app.customer.dto.ChangePwdDto;
import aviaTickets.app.customer.entity.Customer;

// CustomerInteraction -> describe the main User interaction logic
public interface CustomerInteraction {
  // check if user exists 
  public Boolean isCustomerExists(String email);
  // create user
  public void createCustomer(String name, String password, String email);
  // get user data by id 
  public Optional<Customer> getCustomer(Integer id);
  // get user data by email
  public Optional<Customer> getCustomer(String email);
  // get user list 
  public List<Customer> getAll();
  // update user data by <id> key with dto as second argument 
  public void updateProfile(Customer c, Integer id);
  // handle forgot password route and send new password to current user email
  public void changePassword(ChangePwdDto dto);
  // delete user by id
  public void deleteCustomer(Integer idToDelete, Integer customerId);
}