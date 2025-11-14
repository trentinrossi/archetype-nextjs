'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { customerService } from '@/services/customerService';
import { Account, Customer, UpdateAccountRequest, UpdateCustomerRequest } from '@/types/account';
import { Input, Button } from '@/components/ui';

export default function AccountUpdatePage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [customer, setCustomer] = useState<Customer | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  const [accountFormData, setAccountFormData] = useState<UpdateAccountRequest>({});
  const [customerFormData, setCustomerFormData] = useState<UpdateCustomerRequest>({});

  const fetchData = useCallback(async () => {
    if (!params.id) return;

    try {
      setLoading(true);
      setError(null);
      const data = await accountService.getAccountWithCustomer(params.id as string);
      
      setAccount(data.account);
      setCustomer(data.customer);
      
      setAccountFormData({
        activeStatus: data.account.activeStatus,
        currentBalance: data.account.currentBalance,
        creditLimit: data.account.creditLimit,
        cashCreditLimit: data.account.cashCreditLimit,
        openDate: data.account.openDate,
        expirationDate: data.account.expirationDate,
        reissueDate: data.account.reissueDate,
        currentCycleCredit: data.account.currentCycleCredit,
        currentCycleDebit: data.account.currentCycleDebit,
        groupId: data.account.groupId,
      });
      
      setCustomerFormData({
        firstName: data.customer.firstName,
        middleName: data.customer.middleName,
        lastName: data.customer.lastName,
        addressLine1: data.customer.addressLine1,
        addressLine2: data.customer.addressLine2,
        addressLine3: data.customer.addressLine3,
        stateCode: data.customer.stateCode,
        countryCode: data.customer.countryCode,
        zipCode: data.customer.zipCode,
        phoneNumber1: data.customer.phoneNumber1,
        phoneNumber2: data.customer.phoneNumber2,
        ssn: data.customer.ssn,
        governmentIssuedId: data.customer.governmentIssuedId,
        dateOfBirth: data.customer.dateOfBirth,
        eftAccountId: data.customer.eftAccountId,
        primaryCardholderIndicator: data.customer.primaryCardholderIndicator,
        ficoScore: data.customer.ficoScore,
      });
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to load account data';
      setError(errorMessage);
      console.error('Error fetching data:', err);
    } finally {
      setLoading(false);
    }
  }, [params.id]);

  useEffect(() => {
    fetchData();
  }, [fetchData]);

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!accountFormData.activeStatus || !['Y', 'N'].includes(accountFormData.activeStatus)) {
      errors.activeStatus = 'Active status must be Y or N';
    }

    if (accountFormData.currentBalance !== undefined && accountFormData.currentBalance < 0) {
      errors.currentBalance = 'Current balance cannot be negative';
    }

    if (accountFormData.creditLimit !== undefined && accountFormData.creditLimit < 0) {
      errors.creditLimit = 'Credit limit cannot be negative';
    }

    if (accountFormData.cashCreditLimit !== undefined && accountFormData.cashCreditLimit < 0) {
      errors.cashCreditLimit = 'Cash credit limit cannot be negative';
    }

    if (accountFormData.currentCycleCredit !== undefined && accountFormData.currentCycleCredit < 0) {
      errors.currentCycleCredit = 'Current cycle credit cannot be negative';
    }

    if (accountFormData.currentCycleDebit !== undefined && accountFormData.currentCycleDebit < 0) {
      errors.currentCycleDebit = 'Current cycle debit cannot be negative';
    }

    if (accountFormData.groupId && accountFormData.groupId.length > 10) {
      errors.groupId = 'Group ID cannot exceed 10 characters';
    }

    if (!customerFormData.firstName || customerFormData.firstName.trim().length === 0) {
      errors.firstName = 'First name is required';
    } else if (customerFormData.firstName.length > 25) {
      errors.firstName = 'First name cannot exceed 25 characters';
    }

    if (customerFormData.middleName && customerFormData.middleName.length > 25) {
      errors.middleName = 'Middle name cannot exceed 25 characters';
    }

    if (!customerFormData.lastName || customerFormData.lastName.trim().length === 0) {
      errors.lastName = 'Last name is required';
    } else if (customerFormData.lastName.length > 25) {
      errors.lastName = 'Last name cannot exceed 25 characters';
    }

    if (!customerFormData.addressLine1 || customerFormData.addressLine1.trim().length === 0) {
      errors.addressLine1 = 'Address line 1 is required';
    } else if (customerFormData.addressLine1.length > 50) {
      errors.addressLine1 = 'Address line 1 cannot exceed 50 characters';
    }

    if (customerFormData.addressLine2 && customerFormData.addressLine2.length > 50) {
      errors.addressLine2 = 'Address line 2 cannot exceed 50 characters';
    }

    if (!customerFormData.addressLine3 || customerFormData.addressLine3.trim().length === 0) {
      errors.addressLine3 = 'City is required';
    } else if (customerFormData.addressLine3.length > 50) {
      errors.addressLine3 = 'City cannot exceed 50 characters';
    }

    if (!customerFormData.stateCode || customerFormData.stateCode.length !== 2) {
      errors.stateCode = 'State code must be 2 characters';
    }

    if (!customerFormData.countryCode || customerFormData.countryCode.length !== 3) {
      errors.countryCode = 'Country code must be 3 characters';
    }

    if (!customerFormData.zipCode || customerFormData.zipCode.trim().length === 0) {
      errors.zipCode = 'ZIP code is required';
    } else if (customerFormData.zipCode.length > 10) {
      errors.zipCode = 'ZIP code cannot exceed 10 characters';
    }

    if (!customerFormData.phoneNumber1 || customerFormData.phoneNumber1.trim().length === 0) {
      errors.phoneNumber1 = 'Primary phone number is required';
    }

    if (customerFormData.ssn !== undefined) {
      const ssnStr = customerFormData.ssn.toString();
      if (ssnStr.length !== 9) {
        errors.ssn = 'SSN must be 9 digits';
      }
    }

    if (customerFormData.ficoScore !== undefined) {
      if (customerFormData.ficoScore < 300 || customerFormData.ficoScore > 850) {
        errors.ficoScore = 'FICO score must be between 300 and 850';
      }
    }

    if (!customerFormData.primaryCardholderIndicator || !['Y', 'N'].includes(customerFormData.primaryCardholderIndicator)) {
      errors.primaryCardholderIndicator = 'Primary cardholder indicator must be Y or N';
    }

    if (customerFormData.dateOfBirth) {
      const dob = new Date(customerFormData.dateOfBirth);
      const today = new Date();
      const age = today.getFullYear() - dob.getFullYear();
      if (age < 18 || age > 120) {
        errors.dateOfBirth = 'Customer must be between 18 and 120 years old';
      }
    }

    setValidationErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      setError('Please fix validation errors before submitting');
      return;
    }

    if (!account || !customer) return;

    try {
      setSaving(true);
      setError(null);
      setSuccessMessage(null);

      await accountService.updateAccount(account.accountId.toString(), accountFormData);
      await customerService.updateCustomer(customer.customerId.toString(), customerFormData);

      setSuccessMessage('Account and customer information updated successfully');
      
      await fetchData();
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to update account';
      setError(errorMessage);
      console.error('Error updating data:', err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    router.push('/accounts');
  };

  if (loading) {
    return (
      <div className="p-6">
        <div className="text-center">Loading account data...</div>
      </div>
    );
  }

  if (error && !account) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
        <Button variant="secondary" onClick={() => router.push('/accounts')}>
          Back to Accounts
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-6xl mx-auto">
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Account Update</h1>
        <p className="text-gray-600 mt-1">Update account and customer information</p>
      </div>

      {successMessage && (
        <div className="bg-green-50 border border-green-200 text-green-700 px-4 py-3 rounded mb-4">
          {successMessage}
        </div>
      )}

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
      )}

      <form onSubmit={handleSubmit} className="space-y-6">
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4">Account Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Input
              label="Account ID"
              value={account?.accountId || ''}
              disabled
              className="bg-gray-50"
            />

            <Input
              label="Active Status (Y/N)"
              value={accountFormData.activeStatus || ''}
              onChange={(e) => {
                const value = e.target.value.toUpperCase();
                setAccountFormData({ ...accountFormData, activeStatus: value });
                if (validationErrors.activeStatus) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.activeStatus;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={1}
              error={validationErrors.activeStatus}
            />

            <Input
              label="Current Balance"
              type="number"
              step="0.01"
              value={accountFormData.currentBalance || ''}
              onChange={(e) => {
                setAccountFormData({ ...accountFormData, currentBalance: parseFloat(e.target.value) });
                if (validationErrors.currentBalance) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.currentBalance;
                  setValidationErrors(newErrors);
                }
              }}
              required
              error={validationErrors.currentBalance}
            />

            <Input
              label="Credit Limit"
              type="number"
              step="0.01"
              value={accountFormData.creditLimit || ''}
              onChange={(e) => {
                setAccountFormData({ ...accountFormData, creditLimit: parseFloat(e.target.value) });
                if (validationErrors.creditLimit) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.creditLimit;
                  setValidationErrors(newErrors);
                }
              }}
              required
              error={validationErrors.creditLimit}
            />

            <Input
              label="Cash Credit Limit"
              type="number"
              step="0.01"
              value={accountFormData.cashCreditLimit || ''}
              onChange={(e) => {
                setAccountFormData({ ...accountFormData, cashCreditLimit: parseFloat(e.target.value) });
                if (validationErrors.cashCreditLimit) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.cashCreditLimit;
                  setValidationErrors(newErrors);
                }
              }}
              required
              error={validationErrors.cashCreditLimit}
            />

            <Input
              label="Open Date"
              type="date"
              value={accountFormData.openDate || ''}
              onChange={(e) => setAccountFormData({ ...accountFormData, openDate: e.target.value })}
              required
            />

            <Input
              label="Expiration Date"
              type="date"
              value={accountFormData.expirationDate || ''}
              onChange={(e) => setAccountFormData({ ...accountFormData, expirationDate: e.target.value })}
              required
            />

            <Input
              label="Reissue Date"
              type="date"
              value={accountFormData.reissueDate || ''}
              onChange={(e) => setAccountFormData({ ...accountFormData, reissueDate: e.target.value })}
              required
            />

            <Input
              label="Current Cycle Credit"
              type="number"
              step="0.01"
              value={accountFormData.currentCycleCredit || ''}
              onChange={(e) => {
                setAccountFormData({ ...accountFormData, currentCycleCredit: parseFloat(e.target.value) });
                if (validationErrors.currentCycleCredit) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.currentCycleCredit;
                  setValidationErrors(newErrors);
                }
              }}
              required
              error={validationErrors.currentCycleCredit}
            />

            <Input
              label="Current Cycle Debit"
              type="number"
              step="0.01"
              value={accountFormData.currentCycleDebit || ''}
              onChange={(e) => {
                setAccountFormData({ ...accountFormData, currentCycleDebit: parseFloat(e.target.value) });
                if (validationErrors.currentCycleDebit) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.currentCycleDebit;
                  setValidationErrors(newErrors);
                }
              }}
              required
              error={validationErrors.currentCycleDebit}
            />

            <Input
              label="Group ID (Optional)"
              value={accountFormData.groupId || ''}
              onChange={(e) => {
                setAccountFormData({ ...accountFormData, groupId: e.target.value });
                if (validationErrors.groupId) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.groupId;
                  setValidationErrors(newErrors);
                }
              }}
              maxLength={10}
              error={validationErrors.groupId}
            />
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4">Customer Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Input
              label="Customer ID"
              value={customer?.customerId || ''}
              disabled
              className="bg-gray-50"
            />

            <Input
              label="First Name"
              value={customerFormData.firstName || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, firstName: e.target.value });
                if (validationErrors.firstName) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.firstName;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={25}
              error={validationErrors.firstName}
            />

            <Input
              label="Middle Name (Optional)"
              value={customerFormData.middleName || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, middleName: e.target.value });
                if (validationErrors.middleName) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.middleName;
                  setValidationErrors(newErrors);
                }
              }}
              maxLength={25}
              error={validationErrors.middleName}
            />

            <Input
              label="Last Name"
              value={customerFormData.lastName || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, lastName: e.target.value });
                if (validationErrors.lastName) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.lastName;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={25}
              error={validationErrors.lastName}
            />

            <Input
              label="Address Line 1"
              value={customerFormData.addressLine1 || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, addressLine1: e.target.value });
                if (validationErrors.addressLine1) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.addressLine1;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={50}
              error={validationErrors.addressLine1}
            />

            <Input
              label="Address Line 2 (Optional)"
              value={customerFormData.addressLine2 || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, addressLine2: e.target.value });
                if (validationErrors.addressLine2) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.addressLine2;
                  setValidationErrors(newErrors);
                }
              }}
              maxLength={50}
              error={validationErrors.addressLine2}
            />

            <Input
              label="City"
              value={customerFormData.addressLine3 || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, addressLine3: e.target.value });
                if (validationErrors.addressLine3) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.addressLine3;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={50}
              error={validationErrors.addressLine3}
            />

            <Input
              label="State Code"
              value={customerFormData.stateCode || ''}
              onChange={(e) => {
                const value = e.target.value.toUpperCase();
                setCustomerFormData({ ...customerFormData, stateCode: value });
                if (validationErrors.stateCode) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.stateCode;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={2}
              error={validationErrors.stateCode}
            />

            <Input
              label="Country Code"
              value={customerFormData.countryCode || ''}
              onChange={(e) => {
                const value = e.target.value.toUpperCase();
                setCustomerFormData({ ...customerFormData, countryCode: value });
                if (validationErrors.countryCode) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.countryCode;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={3}
              error={validationErrors.countryCode}
            />

            <Input
              label="ZIP Code"
              value={customerFormData.zipCode || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, zipCode: e.target.value });
                if (validationErrors.zipCode) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.zipCode;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={10}
              error={validationErrors.zipCode}
            />

            <Input
              label="Primary Phone Number"
              value={customerFormData.phoneNumber1 || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, phoneNumber1: e.target.value });
                if (validationErrors.phoneNumber1) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.phoneNumber1;
                  setValidationErrors(newErrors);
                }
              }}
              required
              placeholder="(XXX)XXX-XXXX"
              error={validationErrors.phoneNumber1}
            />

            <Input
              label="Secondary Phone Number (Optional)"
              value={customerFormData.phoneNumber2 || ''}
              onChange={(e) => setCustomerFormData({ ...customerFormData, phoneNumber2: e.target.value })}
              placeholder="(XXX)XXX-XXXX"
            />

            <Input
              label="SSN"
              type="number"
              value={customerFormData.ssn || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, ssn: parseInt(e.target.value) });
                if (validationErrors.ssn) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.ssn;
                  setValidationErrors(newErrors);
                }
              }}
              required
              error={validationErrors.ssn}
            />

            <Input
              label="Government Issued ID (Optional)"
              value={customerFormData.governmentIssuedId || ''}
              onChange={(e) => setCustomerFormData({ ...customerFormData, governmentIssuedId: e.target.value })}
              maxLength={20}
            />

            <Input
              label="Date of Birth"
              type="date"
              value={customerFormData.dateOfBirth || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, dateOfBirth: e.target.value });
                if (validationErrors.dateOfBirth) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.dateOfBirth;
                  setValidationErrors(newErrors);
                }
              }}
              required
              error={validationErrors.dateOfBirth}
            />

            <Input
              label="EFT Account ID (Optional)"
              value={customerFormData.eftAccountId || ''}
              onChange={(e) => setCustomerFormData({ ...customerFormData, eftAccountId: e.target.value })}
              maxLength={10}
            />

            <Input
              label="Primary Cardholder (Y/N)"
              value={customerFormData.primaryCardholderIndicator || ''}
              onChange={(e) => {
                const value = e.target.value.toUpperCase();
                setCustomerFormData({ ...customerFormData, primaryCardholderIndicator: value });
                if (validationErrors.primaryCardholderIndicator) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.primaryCardholderIndicator;
                  setValidationErrors(newErrors);
                }
              }}
              required
              maxLength={1}
              error={validationErrors.primaryCardholderIndicator}
            />

            <Input
              label="FICO Score"
              type="number"
              value={customerFormData.ficoScore || ''}
              onChange={(e) => {
                setCustomerFormData({ ...customerFormData, ficoScore: parseInt(e.target.value) });
                if (validationErrors.ficoScore) {
                  const newErrors = { ...validationErrors };
                  delete newErrors.ficoScore;
                  setValidationErrors(newErrors);
                }
              }}
              required
              min={300}
              max={850}
              error={validationErrors.ficoScore}
            />
          </div>
        </div>

        <div className="flex gap-3">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Confirm Update'}
          </Button>
          <Button type="button" variant="secondary" onClick={handleCancel}>
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
