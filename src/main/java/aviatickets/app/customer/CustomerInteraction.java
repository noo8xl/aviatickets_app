package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.entity.Customer;

// CustomerInteraction -> describe the main User interaction logic
public interface CustomerInteraction {
  // check if user exists
  void isCustomerExists(String email);

  // create user
  void createCustomer(String name, String password, String email);

  // get user data by id
  Customer getCustomer(Integer id);

  // get user data by email
  Customer getCustomer(String email);


  // update user data by <id> key with dto as second argument
  void updateProfile(Integer id, Customer c);

  // handle forgot password route and send new password to current user email
  Integer changePassword(String email, String password);

	// enable OR disable user 2fa status
	void change2faStatus(ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException;

	// ##########################################################################################################
	// ##################################### ADMIN permission only ##############################################
	// ##########################################################################################################

	// get user list
	List<Customer> getAll(Short skip, Short limit);

	// delete user by id (available ONLY for ADMIN role customer)
  void deleteCustomer(Integer idToDelete, Integer customerId) throws SQLException, ClassNotFoundException;
}