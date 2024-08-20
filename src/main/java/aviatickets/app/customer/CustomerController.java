package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.dto.UpdateCustomerDto;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import aviatickets.app.customer.dto.ChangePwdDto;
import aviatickets.app.customer.entity.Customer;
import jakarta.validation.Valid;

@RequiredArgsConstructor
@RestController
@RequestMapping("/customer")
public class CustomerController {

  private final CustomerInterface customerService;

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get-customer-by-email/{email}/")
  public ResponseEntity<Customer> findOne(@PathVariable String email) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.customerService.findOne(email));
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get-customer-by-id/{id}/")
  public ResponseEntity<Customer> findOne(@PathVariable Integer id) throws SQLException, ClassNotFoundException {
		return ResponseEntity.ok(this.customerService.findOne(id));
  }

  @ResponseStatus(HttpStatus.NO_CONTENT)
  @PutMapping("/update/update-customer-details/")
  public void update(@Valid @RequestBody UpdateCustomerDto dto) throws SQLException, ClassNotFoundException {
		this.customerService.updateProfile(dto);
  }

  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/update/change-password/")
  public void changePassword(@RequestBody ChangePwdDto dto) throws SQLException, ClassNotFoundException {
		this.customerService.updatePassword(dto.email(), dto.pwd());
  }


	// changeTwoStepStatus -> change user 2fa status (on/off)
	@ResponseStatus(HttpStatus.ACCEPTED)
	@PatchMapping("/update/change-2fa-status/")
	public void changeTwoStepStatus(@RequestBody ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException {
		this.customerService.update2faStatus(dto);
	}


// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################


//	@ResponseStatus(HttpStatus.CREATED)
//	@PostMapping("/create/")
//	public void create(@Valid @RequestBody Customer c) throws SQLException, ClassNotFoundException {
//		customerService.createCustomer(c.name(), c.password(), c.email());
//	}



	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-customer-list/{skip}/{limit}/")
	public ResponseEntity<List<Customer>> findAll( @PathVariable Integer skip, @PathVariable Integer limit) throws SQLException, ClassNotFoundException {
		System.out.println("Customer Controller findAll");
		return ResponseEntity.ok(this.customerService.findAll(skip, limit));
	}


	@ResponseStatus(HttpStatus.ACCEPTED)
	@PatchMapping("/update-ban-status/{customerId}/{status}/")
	public void updateBanStatus(@PathVariable Integer customerId, @PathVariable Boolean status) throws SQLException, ClassNotFoundException {
		this.customerService.updateBanStatus(customerId, status);
	}

	@ResponseStatus(HttpStatus.NO_CONTENT)
	@DeleteMapping("/delete/{idToDelete}/{adminId}/")
	public void delete(@PathVariable Integer idToDelete, @PathVariable Integer adminId) throws SQLException, ClassNotFoundException {
		this.customerService.deleteCustomer(idToDelete, adminId);
	}

}
