package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.dto.UpdateCustomerDto;
import aviatickets.app.customer.entity.Customer;

// CustomerInteraction -> describe the main Customer interaction logic
interface CustomerInteraction {

  // create customer
	void save(String name, String password, String email) throws SQLException, ClassNotFoundException;

  // get customer data by id
  Customer findOne(Integer id) throws SQLException, ClassNotFoundException;

  // get customer data by email
  Customer findOne(String email) throws SQLException, ClassNotFoundException;

  // update customer profile data
  void updateProfile(UpdateCustomerDto dto) throws SQLException, ClassNotFoundException;

  // handle forgot password route and send new password to current customer email
  Integer updatePassword(String email, String password) throws SQLException, ClassNotFoundException;

	// enable OR disable customer 2fa status
	void update2faStatus(ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException;

	// get customer 2fa status data
	Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException;


	// ##########################################################################################################
	// ##################################### ADMIN permission only ##############################################
	// ##########################################################################################################

  // -> permissions will check by application security service

	// set customer isBanned status as ADMIN
	void updateBanStatus(Integer customerId, Boolean status) throws SQLException, ClassNotFoundException;

	// get a list of customers as ADMIN
	List<Customer> findAll(Integer skip, Integer limit) throws SQLException, ClassNotFoundException;

	// delete customer by id as ADMIN
  void deleteCustomer(Integer idToDelete, Integer customerId) throws SQLException, ClassNotFoundException;
}