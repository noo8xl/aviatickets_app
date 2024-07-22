package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import aviatickets.app.customer.dto.ChangePwdDto;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.NotFoundException;
import aviatickets.app.exception.ServerErrorException;
import jakarta.validation.Valid;

@RestController
@RequestMapping("/customer")
public class CustomerController {

  private final CustomerService customerService;

  public CustomerController(CustomerService customerService) {
    this.customerService = customerService;
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get/{email}/")
  public Customer findOne(@PathVariable String email) {
		return customerService.getCustomer(email);
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get/{id}/")
  public Customer findOne(@PathVariable Integer id) {
		return customerService.getCustomer(id);
  }

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/create/")
  public void create(@Valid @RequestBody Customer customer) {
		customerService.createCustomer(customer.name(), customer.password(), customer.email());
  }

  @ResponseStatus(HttpStatus.NO_CONTENT)
  @PutMapping("/update/{id}/")
  public void update(@Valid @RequestBody Customer customer, @PathVariable Integer id) {
		customerService.updateProfile(id, customer);
  }

  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/change-password/")
  public void changePassword(@RequestBody ChangePwdDto dto) {
		customerService.changePassword(dto.email(), dto.pwd());
  }


	// changeTwoStepStatus -> change user 2fa status (on/off)
	@ResponseStatus(HttpStatus.ACCEPTED)
	@PostMapping("/change_2fa_status/")
	public void changeTwoStepStatus(@RequestBody ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException {
		customerService.change2faStatus(dto);
	}


// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################


	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-customer-list/{skip}/{limit}/")
	public List<Customer> findAll(@PathVariable Short skip, @PathVariable Short limit) {
		List<Customer> customers = customerService.getAll(skip, limit);
		if (customers.isEmpty()) {
			throw new NotFoundException("Empty set.");
		} else return customers;
	}

	@ResponseStatus(HttpStatus.NO_CONTENT)
	@DeleteMapping("/delete/{idToDelete}/{customerId}")
	public void delete(@PathVariable Integer idToDelete, @PathVariable Integer customerId) {
		try {
			customerService.deleteCustomer(idToDelete, customerId);
		} catch (Exception e) {
			throw new ServerErrorException("catch an error at delete user " + e.getCause());
		}
	}



}
