'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { customerService } from '@/services/customerService';
import { CreateAccountRequest } from '@/types/account';
import { CreateCustomerRequest } from '@/types/customer';
import { Input, Button } from '@/components/ui';

export default function CreateAccountPage() {
  const router = useRouter();
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});

  const [accountFormData, setAccountFormData] = useState<CreateAccountRequest>({
    accountId: 0,
    activeStatus: 'Y',
    currentBalance: 0,
    creditLimit: 0,
    cashCreditLimit: 0,
    openDate: new Date().toISOString().split('T')[0],
    expirationDate: '',
    reissueDate: '',
    currentCycleCredit: 0,
    currentCycleDebit: 0,
    groupId: '',
    customerId: 0,
    accountStatus: 'A',
  });

  const [customerFormData, setCustomerFormData] = useState<CreateCustomerRequest>({
    customerId: 0,
    firstName: '',
    middleName: '',
    lastName: '',
    addressLine1: '',
    addressLine2: '',
    addressLine3: '',
    stateCode: '',
    countryCode: 'USA',
    zipCode: '',
    phoneNumber1: '',
    phoneNumber2: '',
    ssn: 0,
    governmentIssuedId: '',
    dateOfBirth: '',
    eftAccountId: '',
    primaryCardholderIndicator: 'Y',
    ficoScore: 300,
    ficoCreditScore: 300,
    city: '',
    primaryPhoneNumber: '',
    secondaryPhoneNumber: '',
  });

  const validateAccountFields = (): boolean => {
    const errors: Record<string, string> = {};

    if (!accountFormData.accountId || accountFormData.accountId === 0) {
      errors.accountId = 'Account ID is required';
    } else if (accountFormData.accountId.toString().length !== 11) {
      errors.accountId = 'Account number must be 11 digits';
    }

    if (!accountFormData.activeStatus || !['Y', 'N'].includes(accountFormData.activeStatus)) {
      errors.activeStatus = 'Account status must be Y or N';
    }

    if (accountFormData.currentBalance < 0) {
      errors.currentBalance = 'Current balance must be a valid positive number';
    }

    if (accountFormData.creditLimit < 0) {
      errors.creditLimit = 'Credit limit must be a valid positive number';
    }

    if (accountFormData.cashCreditLimit < 0) {
      errors.cashCreditLimit = 'Cash credit limit must be a valid positive number';
    }

    if (accountFormData.currentCycleCredit < 0) {
      errors.currentCycleCredit = 'Current cycle credit must be a valid positive number';
    }

    if (accountFormData.currentCycleDebit < 0) {
      errors.currentCycleDebit = 'Current cycle debit must be a valid positive number';
    }

    if (!accountFormData.openDate) {
      errors.openDate = 'Open date is required';
    }

    if (!accountFormData.expirationDate) {
      errors.expirationDate = 'Expiration date is required';
    }

    if (!accountFormData.reissueDate) {
      errors.reissueDate = 'Reissue date is required';
    }

    if (!accountFormData.customerId || accountFormData.customerId === 0) {
      errors.customerId = 'Customer ID is required';
    } else if (accountFormData.customerId.toString().length !== 9) {
      errors.customerId = 'Customer ID must be 9 digits';
    }

    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const validateCustomerFields = (): boolean => {
    const errors: Record<string, string> = {};

    if (!customerFormData.customerId || customerFormData.customerId === 0) {
      errors.customerId = 'Customer ID is required';
    } else if (customerFormData.customerId.toString().length !== 9) {
      errors.customerId = 'Customer ID must be 9 digits';
    }

    if (!customerFormData.firstName) {
      errors.firstName = 'First name is required';
    } else if (!/^[A-Za-z\s]+$/.test(customerFormData.firstName)) {
      errors.firstName = 'First name must contain only letters';
    }

    if (!customerFormData.lastName) {
      errors.lastName = 'Last name is required';
    } else if (!/^[A-Za-z\s]+$/.test(customerFormData.lastName)) {
      errors.lastName = 'Last name must contain only letters';
    }

    if (!customerFormData.stateCode) {
      errors.stateCode = 'State code is required';
    } else if (!/^[A-Z]{2}$/.test(customerFormData.stateCode)) {
      errors.stateCode = 'Invalid state code';
    }

    if (!customerFormData.countryCode) {
      errors.countryCode = 'Country code is required';
    } else if (!/^[A-Z]{3}$/.test(customerFormData.countryCode)) {
      errors.countryCode = 'Invalid country code';
    }

    if (!customerFormData.zipCode) {
      errors.zipCode = 'ZIP code is required';
    } else if (!/^\d{5}$/.test(customerFormData.zipCode)) {
      errors.zipCode = 'Invalid zip code';
    }

    if (!customerFormData.primaryPhoneNumber) {
      errors.primaryPhoneNumber = 'Primary phone is required';
    } else if (!/^\(\d{3}\)\d{3}-\d{4}$/.test(customerFormData.primaryPhoneNumber)) {
      errors.primaryPhoneNumber = 'Invalid phone number format. Use (XXX)XXX-XXXX';
    }

    if (customerFormData.secondaryPhoneNumber && !/^\(\d{3}\)\d{3}-\d{4}$/.test(customerFormData.secondaryPhoneNumber)) {
      errors.secondaryPhoneNumber = 'Invalid phone number format. Use (XXX)XXX-XXXX';
    }

    if (!customerFormData.ssn || customerFormData.ssn === 0) {
      errors.ssn = 'SSN is required';
    } else if (customerFormData.ssn.toString().length !== 9) {
      errors.ssn = 'Invalid Social Security Number';
    }

    if (!customerFormData.dateOfBirth) {
      errors.dateOfBirth = 'Date of birth is required';
    }

    if (customerFormData.ficoScore < 300 || customerFormData.ficoScore > 850) {
      errors.ficoScore = 'FICO Score should be between 300 and 850';
    }

    if (!customerFormData.primaryCardholderIndicator || !['Y', 'N'].includes(customerFormData.primaryCardholderIndicator)) {
      errors.primaryCardholderIndicator = 'Primary cardholder indicator must be Y or N';
    }

    if (!customerFormData.addressLine1) {
      errors.addressLine1 = 'Address line 1 is required';
    }

    if (!customerFormData.city) {
      errors.city = 'City is required';
    }

    setFieldErrors((prev) => ({ ...prev, ...errors }));
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);
    setFieldErrors({});

    const accountValid = validateAccountFields();
    const customerValid = validateCustomerFields();

    if (!accountValid || !customerValid) {
      setError('Please correct the validation errors');
      return;
    }

    try {
      setLoading(true);

      await customerService.createCustomer({
        ...customerFormData,
        phoneNumber1: customerFormData.primaryPhoneNumber,
        phoneNumber2: customerFormData.secondaryPhoneNumber || '',
      });

      await accountService.createAccount(accountFormData);

      router.push('/accounts');
    } catch (err: any) {
      setError(err.message || 'Failed to create account');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-7xl mx-auto">
      <h1 className="text-2xl font-bold mb-6">Create New Account</h1>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-6">
          {error}
        </div>
      )}
      
      <form onSubmit={handleSubmit}>
        <div className="bg-white shadow rounded-lg p-6 mb-6">
          <h2 className="text-lg font-semibold mb-4">Account Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            <div>
              <Input
                label="Account ID"
                type="number"
                value={accountFormData.accountId || ''}
                onChange={(e) => setAccountFormData({ ...accountFormData, accountId: parseInt(e.target.value) || 0 })}
                required
                className={fieldErrors.accountId ? 'border-red-500' : ''}
              />
              {fieldErrors.accountId && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.accountId}</p>
              )}
            </div>
            <div>
              <Input
                label="Active Status (Y/N)"
                value={accountFormData.activeStatus}
                onChange={(e) => setAccountFormData({ ...accountFormData, activeStatus: e.target.value.toUpperCase() })}
                required
                className={fieldErrors.activeStatus ? 'border-red-500' : ''}
              />
              {fieldErrors.activeStatus && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.activeStatus}</p>
              )}
            </div>
            <div>
              <Input
                label="Current Balance"
                type="number"
                step="0.01"
                value={accountFormData.currentBalance}
                onChange={(e) => setAccountFormData({ ...accountFormData, currentBalance: parseFloat(e.target.value) || 0 })}
                required
                className={fieldErrors.currentBalance ? 'border-red-500' : ''}
              />
              {fieldErrors.currentBalance && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.currentBalance}</p>
              )}
            </div>
            <div>
              <Input
                label="Credit Limit"
                type="number"
                step="0.01"
                value={accountFormData.creditLimit}
                onChange={(e) => setAccountFormData({ ...accountFormData, creditLimit: parseFloat(e.target.value) || 0 })}
                required
                className={fieldErrors.creditLimit ? 'border-red-500' : ''}
              />
              {fieldErrors.creditLimit && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.creditLimit}</p>
              )}
            </div>
            <div>
              <Input
                label="Cash Credit Limit"
                type="number"
                step="0.01"
                value={accountFormData.cashCreditLimit}
                onChange={(e) => setAccountFormData({ ...accountFormData, cashCreditLimit: parseFloat(e.target.value) || 0 })}
                required
                className={fieldErrors.cashCreditLimit ? 'border-red-500' : ''}
              />
              {fieldErrors.cashCreditLimit && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.cashCreditLimit}</p>
              )}
            </div>
            <div>
              <Input
                label="Open Date"
                type="date"
                value={accountFormData.openDate}
                onChange={(e) => setAccountFormData({ ...accountFormData, openDate: e.target.value })}
                required
                className={fieldErrors.openDate ? 'border-red-500' : ''}
              />
              {fieldErrors.openDate && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.openDate}</p>
              )}
            </div>
            <div>
              <Input
                label="Expiration Date"
                type="date"
                value={accountFormData.expirationDate}
                onChange={(e) => setAccountFormData({ ...accountFormData, expirationDate: e.target.value })}
                required
                className={fieldErrors.expirationDate ? 'border-red-500' : ''}
              />
              {fieldErrors.expirationDate && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.expirationDate}</p>
              )}
            </div>
            <div>
              <Input
                label="Reissue Date"
                type="date"
                value={accountFormData.reissueDate}
                onChange={(e) => setAccountFormData({ ...accountFormData, reissueDate: e.target.value })}
                required
                className={fieldErrors.reissueDate ? 'border-red-500' : ''}
              />
              {fieldErrors.reissueDate && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.reissueDate}</p>
              )}
            </div>
            <div>
              <Input
                label="Current Cycle Credit"
                type="number"
                step="0.01"
                value={accountFormData.currentCycleCredit}
                onChange={(e) => setAccountFormData({ ...accountFormData, currentCycleCredit: parseFloat(e.target.value) || 0 })}
                required
                className={fieldErrors.currentCycleCredit ? 'border-red-500' : ''}
              />
              {fieldErrors.currentCycleCredit && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.currentCycleCredit}</p>
              )}
            </div>
            <div>
              <Input
                label="Current Cycle Debit"
                type="number"
                step="0.01"
                value={accountFormData.currentCycleDebit}
                onChange={(e) => setAccountFormData({ ...accountFormData, currentCycleDebit: parseFloat(e.target.value) || 0 })}
                required
                className={fieldErrors.currentCycleDebit ? 'border-red-500' : ''}
              />
              {fieldErrors.currentCycleDebit && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.currentCycleDebit}</p>
              )}
            </div>
            <Input
              label="Group ID"
              value={accountFormData.groupId}
              onChange={(e) => setAccountFormData({ ...accountFormData, groupId: e.target.value })}
            />
            <div>
              <Input
                label="Customer ID"
                type="number"
                value={accountFormData.customerId || ''}
                onChange={(e) => {
                  const customerId = parseInt(e.target.value) || 0;
                  setAccountFormData({ ...accountFormData, customerId });
                  setCustomerFormData({ ...customerFormData, customerId });
                }}
                required
                className={fieldErrors.customerId ? 'border-red-500' : ''}
              />
              {fieldErrors.customerId && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.customerId}</p>
              )}
            </div>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6 mb-6">
          <h2 className="text-lg font-semibold mb-4">Customer Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            <div>
              <Input
                label="First Name"
                value={customerFormData.firstName}
                onChange={(e) => setCustomerFormData({ ...customerFormData, firstName: e.target.value })}
                required
                className={fieldErrors.firstName ? 'border-red-500' : ''}
              />
              {fieldErrors.firstName && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.firstName}</p>
              )}
            </div>
            <Input
              label="Middle Name"
              value={customerFormData.middleName}
              onChange={(e) => setCustomerFormData({ ...customerFormData, middleName: e.target.value })}
            />
            <div>
              <Input
                label="Last Name"
                value={customerFormData.lastName}
                onChange={(e) => setCustomerFormData({ ...customerFormData, lastName: e.target.value })}
                required
                className={fieldErrors.lastName ? 'border-red-500' : ''}
              />
              {fieldErrors.lastName && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.lastName}</p>
              )}
            </div>
            <div>
              <Input
                label="Address Line 1"
                value={customerFormData.addressLine1}
                onChange={(e) => setCustomerFormData({ ...customerFormData, addressLine1: e.target.value })}
                required
                className={fieldErrors.addressLine1 ? 'border-red-500' : ''}
              />
              {fieldErrors.addressLine1 && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.addressLine1}</p>
              )}
            </div>
            <Input
              label="Address Line 2"
              value={customerFormData.addressLine2}
              onChange={(e) => setCustomerFormData({ ...customerFormData, addressLine2: e.target.value })}
            />
            <Input
              label="Address Line 3"
              value={customerFormData.addressLine3}
              onChange={(e) => setCustomerFormData({ ...customerFormData, addressLine3: e.target.value })}
            />
            <div>
              <Input
                label="City"
                value={customerFormData.city}
                onChange={(e) => setCustomerFormData({ ...customerFormData, city: e.target.value })}
                required
                className={fieldErrors.city ? 'border-red-500' : ''}
              />
              {fieldErrors.city && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.city}</p>
              )}
            </div>
            <div>
              <Input
                label="State Code"
                value={customerFormData.stateCode}
                onChange={(e) => setCustomerFormData({ ...customerFormData, stateCode: e.target.value.toUpperCase() })}
                required
                className={fieldErrors.stateCode ? 'border-red-500' : ''}
              />
              {fieldErrors.stateCode && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.stateCode}</p>
              )}
            </div>
            <div>
              <Input
                label="ZIP Code"
                value={customerFormData.zipCode}
                onChange={(e) => setCustomerFormData({ ...customerFormData, zipCode: e.target.value })}
                required
                className={fieldErrors.zipCode ? 'border-red-500' : ''}
              />
              {fieldErrors.zipCode && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.zipCode}</p>
              )}
            </div>
            <div>
              <Input
                label="Country Code"
                value={customerFormData.countryCode}
                onChange={(e) => setCustomerFormData({ ...customerFormData, countryCode: e.target.value.toUpperCase() })}
                required
                className={fieldErrors.countryCode ? 'border-red-500' : ''}
              />
              {fieldErrors.countryCode && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.countryCode}</p>
              )}
            </div>
            <div>
              <Input
                label="Primary Phone"
                value={customerFormData.primaryPhoneNumber}
                onChange={(e) => setCustomerFormData({ ...customerFormData, primaryPhoneNumber: e.target.value })}
                placeholder="(XXX)XXX-XXXX"
                required
                className={fieldErrors.primaryPhoneNumber ? 'border-red-500' : ''}
              />
              {fieldErrors.primaryPhoneNumber && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.primaryPhoneNumber}</p>
              )}
            </div>
            <div>
              <Input
                label="Secondary Phone"
                value={customerFormData.secondaryPhoneNumber}
                onChange={(e) => setCustomerFormData({ ...customerFormData, secondaryPhoneNumber: e.target.value })}
                placeholder="(XXX)XXX-XXXX"
                className={fieldErrors.secondaryPhoneNumber ? 'border-red-500' : ''}
              />
              {fieldErrors.secondaryPhoneNumber && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.secondaryPhoneNumber}</p>
              )}
            </div>
            <div>
              <Input
                label="SSN"
                type="number"
                value={customerFormData.ssn || ''}
                onChange={(e) => setCustomerFormData({ ...customerFormData, ssn: parseInt(e.target.value) || 0 })}
                required
                className={fieldErrors.ssn ? 'border-red-500' : ''}
              />
              {fieldErrors.ssn && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.ssn}</p>
              )}
            </div>
            <Input
              label="Government Issued ID"
              value={customerFormData.governmentIssuedId}
              onChange={(e) => setCustomerFormData({ ...customerFormData, governmentIssuedId: e.target.value })}
            />
            <div>
              <Input
                label="Date of Birth"
                type="date"
                value={customerFormData.dateOfBirth}
                onChange={(e) => setCustomerFormData({ ...customerFormData, dateOfBirth: e.target.value })}
                required
                className={fieldErrors.dateOfBirth ? 'border-red-500' : ''}
              />
              {fieldErrors.dateOfBirth && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.dateOfBirth}</p>
              )}
            </div>
            <Input
              label="EFT Account ID"
              value={customerFormData.eftAccountId}
              onChange={(e) => setCustomerFormData({ ...customerFormData, eftAccountId: e.target.value })}
            />
            <div>
              <Input
                label="Primary Cardholder (Y/N)"
                value={customerFormData.primaryCardholderIndicator}
                onChange={(e) => setCustomerFormData({ ...customerFormData, primaryCardholderIndicator: e.target.value.toUpperCase() })}
                required
                className={fieldErrors.primaryCardholderIndicator ? 'border-red-500' : ''}
              />
              {fieldErrors.primaryCardholderIndicator && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.primaryCardholderIndicator}</p>
              )}
            </div>
            <div>
              <Input
                label="FICO Score"
                type="number"
                value={customerFormData.ficoScore}
                onChange={(e) => {
                  const score = parseInt(e.target.value) || 300;
                  setCustomerFormData({ ...customerFormData, ficoScore: score, ficoCreditScore: score });
                }}
                required
                className={fieldErrors.ficoScore ? 'border-red-500' : ''}
              />
              {fieldErrors.ficoScore && (
                <p className="text-red-600 text-sm mt-1">{fieldErrors.ficoScore}</p>
              )}
            </div>
          </div>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Account'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/accounts')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
