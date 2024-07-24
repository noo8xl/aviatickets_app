package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.entity.Customer;

// CustomerInteraction -> describe the main User interaction logic
interface CustomerInteraction {
  // check if user exists
  void isCustomerExists(String email) throws SQLException, ClassNotFoundException;

	// check if user exists
	void isCustomerExists(Integer id) throws SQLException, ClassNotFoundException;

  // create user
  void createCustomer(String name, String password, String email) throws SQLException, ClassNotFoundException;

  // get user data by id
  Customer getCustomer(Integer id) throws SQLException, ClassNotFoundException;

  // get user data by email
  Customer getCustomer(String email) throws SQLException, ClassNotFoundException;

  // update user data by <id> key with dto as second argument
  void updateProfile(Customer c) throws SQLException, ClassNotFoundException;

  // handle forgot password route and send new password to current user email
  Integer changePassword(String email, String password) throws SQLException, ClassNotFoundException;

	// enable OR disable user 2fa status
	void change2faStatus(ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException;

	// get 2fa status data
	Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException;


	// ##########################################################################################################
	// ##################################### ADMIN permission only ##############################################
	// ##########################################################################################################

	// get user list
	List<Customer> getAll(Integer skip, Integer limit) throws SQLException, ClassNotFoundException;

	// delete user by id (available ONLY for ADMIN role customer)
  void deleteCustomer(Integer idToDelete, Integer customerId) throws SQLException, ClassNotFoundException;
}