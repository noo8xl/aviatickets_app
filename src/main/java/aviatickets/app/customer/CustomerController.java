package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.dto.UpdateCustomerDto;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
  @GetMapping("/get-customer-by-email/{email}/")
  public ResponseEntity<Customer> findOne(@PathVariable String email) throws SQLException, ClassNotFoundException {
		this.customerService.isCustomerExists(email);
		return ResponseEntity.ok(this.customerService.getCustomer(email));
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get-customer-by-id/{id}/")
  public ResponseEntity<Customer> findOne(@PathVariable Integer id) throws SQLException, ClassNotFoundException {
		this.customerService.isCustomerExists(id);
		return ResponseEntity.ok(this.customerService.getCustomer(id));
  }

  @ResponseStatus(HttpStatus.NO_CONTENT)
  @PutMapping("/update/")
  public void update(@Valid @RequestBody UpdateCustomerDto dto) throws SQLException, ClassNotFoundException {
		this.customerService.updateProfile(dto);
  }

  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/change-password/")
  public void changePassword(@RequestBody ChangePwdDto dto) throws SQLException, ClassNotFoundException {
		customerService.changePassword(dto.email(), dto.pwd());
  }


	// changeTwoStepStatus -> change user 2fa status (on/off)
	@ResponseStatus(HttpStatus.ACCEPTED)
	@PatchMapping("/change-2fa-status/")
	public void changeTwoStepStatus(@RequestBody ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException {
		customerService.change2faStatus(dto);
	}


// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################


//	@ResponseStatus(HttpStatus.CREATED)
//	@PostMapping("/create/")
//	public void create(@Valid @RequestBody Customer customer) throws SQLException, ClassNotFoundException {
//		customerService.createCustomer(customer.name(), customer.password(), customer.email());
//	}


	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-customer-list/{skip}/{limit}/{adminId}/")
	public ResponseEntity<List<Customer>> findAll(
			@PathVariable Integer skip, @PathVariable Integer limit,
			@PathVariable Integer adminId
			) throws SQLException, ClassNotFoundException {
		List<Customer> customers = this.customerService.getAll(skip, limit, adminId);
		if (customers.isEmpty()) {
			throw new NotFoundException("Empty set.");
		} else return ResponseEntity.ok(customers);
	}

	@ResponseStatus(HttpStatus.NO_CONTENT)
	@DeleteMapping("/delete/{idToDelete}/{adminId}")
	public void delete(@PathVariable Integer idToDelete, @PathVariable Integer adminId) throws SQLException, ClassNotFoundException {
		this.customerService.deleteCustomer(idToDelete, adminId);
	}


	@ResponseStatus(HttpStatus.ACCEPTED)
	@PatchMapping("/update/update-ban-status/{customerId}/{status}/{adminId}/")
	public void updateBanStatus(
			@PathVariable Integer customerId, @PathVariable Boolean status,
			@PathVariable Integer adminId) throws SQLException, ClassNotFoundException {
		this.customerService.changeBanStatus(customerId, status, adminId);
	}

}
