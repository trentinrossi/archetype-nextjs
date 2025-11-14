'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { customerService } from '@/services/customerService';
import { Account, UpdateAccountRequest } from '@/types/account';
import { Customer, UpdateCustomerRequest } from '@/types/customer';
import { Input, Button } from '@/components/ui';

export default function AccountUpdatePage() {
  const router = useRouter();
  const [searchAccountId, setSearchAccountId] = useState('');
  const [account, setAccount] = useState<Account | null>(null);
  const [customer, setCustomer] = useState<Customer | null>(null);
  const [loading, setLoading] = useState(false);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});

  const [accountFormData, setAccountFormData] = useState<UpdateAccountRequest>({});
  const [customerFormData, setCustomerFormData] = useState<UpdateCustomerRequest>({});

  const validateAccountId = (value: string): boolean => {
    if (!value || value.trim() === '') {
      setError('Account number is required');
      return false;
    }
    
    if (!/^\d{11}$/.test(value)) {
      setError('Account number must be 11 digits');
      return false;
    }
    
    if (value === '00000000000') {
      setError('Account number must be non-zero');
      return false;
    }
    
    return true;
  };

  const validateAccountFields = (): boolean => {
    const errors: Record<string, string> = {};

    if (accountFormData.activeStatus && !['Y', 'N'].includes(accountFormData.activeStatus)) {
      errors.activeStatus = 'Account status must be Y or N';
    }

    if (accountFormData.currentBalance !== undefined && accountFormData.currentBalance < 0) {
      errors.currentBalance = 'Current balance must be a valid positive number';
    }

    if (accountFormData.creditLimit !== undefined && accountFormData.creditLimit < 0) {
      errors.creditLimit = 'Credit limit must be a valid positive number';
    }

    if (accountFormData.cashCreditLimit !== undefined && accountFormData.cashCreditLimit < 0) {
      errors.cashCreditLimit = 'Cash credit limit must be a valid positive number';
    }

    if (accountFormData.currentCycleCredit !== undefined && accountFormData.currentCycleCredit < 0) {
      errors.currentCycleCredit = 'Current cycle credit must be a valid positive number';
    }

    if (accountFormData.currentCycleDebit !== undefined && accountFormData.currentCycleDebit < 0) {
      errors.currentCycleDebit = 'Current cycle debit must be a valid positive number';
    }

    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const validateCustomerFields = (): boolean => {
    const errors: Record<string, string> = {};

    if (customerFormData.firstName && !/^[A-Za-z\s]+$/.test(customerFormData.firstName)) {
      errors.firstName = 'First name must contain only letters';
    }

    if (customerFormData.lastName && !/^[A-Za-z\s]+$/.test(customerFormData.lastName)) {
      errors.lastName = 'Last name must contain only letters';
    }

    if (customerFormData.stateCode && !/^[A-Z]{2}$/.test(customerFormData.stateCode)) {
      errors.stateCode = 'Invalid state code';
    }

    if (customerFormData.countryCode && !/^[A-Z]{3}$/.test(customerFormData.countryCode)) {
      errors.countryCode = 'Invalid country code';
    }

    if (customerFormData.zipCode && !/^\d{5}$/.test(customerFormData.zipCode)) {
      errors.zipCode = 'Invalid zip code';
    }

    if (customerFormData.primaryPhoneNumber && !/^\(\d{3}\)\d{3}-\d{4}$/.test(customerFormData.primaryPhoneNumber)) {
      errors.primaryPhoneNumber = 'Invalid phone number format. Use (XXX)XXX-XXXX';
    }

    if (customerFormData.secondaryPhoneNumber && !/^\(\d{3}\)\d{3}-\d{4}$/.test(customerFormData.secondaryPhoneNumber)) {
      errors.secondaryPhoneNumber = 'Invalid phone number format. Use (XXX)XXX-XXXX';
    }

    if (customerFormData.ficoScore !== undefined && (customerFormData.ficoScore < 300 || customerFormData.ficoScore > 850)) {
      errors.ficoScore = 'FICO Score should be between 300 and 850';
    }

    if (customerFormData.primaryCardholderIndicator && !['Y', 'N'].includes(customerFormData.primaryCardholderIndicator)) {
      errors.primaryCardholderIndicator = 'Primary cardholder indicator must be Y or N';
    }

    setFieldErrors((prev) => ({ ...prev, ...errors }));
    return Object.keys(errors).length === 0;
  };

  const handleRetrieveAccount = async () => {
    setError(null);
    setSuccessMessage(null);
    setAccount(null);
    setCustomer(null);
    setAccountFormData({});
    setCustomerFormData({});
    setFieldErrors({});

    if (!validateAccountId(searchAccountId)) {
      return;
    }

    try {
      setLoading(true);
      
      const accountData = await accountService.getAccountById(searchAccountId);
      setAccount(accountData);
      setAccountFormData({
        activeStatus: accountData.activeStatus,
        currentBalance: accountData.currentBalance,
        creditLimit: accountData.creditLimit,
        cashCreditLimit: accountData.cashCreditLimit,
        openDate: accountData.openDate,
        expirationDate: accountData.expirationDate,
        reissueDate: accountData.reissueDate,
        currentCycleCredit: accountData.currentCycleCredit,
        currentCycleDebit: accountData.currentCycleDebit,
        groupId: accountData.groupId,
        customerId: accountData.customerId,
        accountStatus: accountData.accountStatus,
      });
      
      if (accountData.customerId) {
        try {
          const customerData = await customerService.getCustomerById(accountData.customerId);
          setCustomer(customerData);
          setCustomerFormData({
            firstName: customerData.firstName,
            middleName: customerData.middleName,
            lastName: customerData.lastName,
            addressLine1: customerData.addressLine1,
            addressLine2: customerData.addressLine2,
            addressLine3: customerData.addressLine3,
            stateCode: customerData.stateCode,
            countryCode: customerData.countryCode,
            zipCode: customerData.zipCode,
            primaryPhoneNumber: customerData.primaryPhoneNumber || customerData.phoneNumber1,
            secondaryPhoneNumber: customerData.secondaryPhoneNumber || customerData.phoneNumber2,
            ssn: customerData.ssnRaw,
            governmentIssuedId: customerData.governmentIssuedId,
            dateOfBirth: customerData.dateOfBirth,
            eftAccountId: customerData.eftAccountId,
            primaryCardholderIndicator: customerData.primaryCardholderIndicator,
            ficoScore: customerData.ficoScore,
            ficoCreditScore: customerData.ficoCreditScore,
            city: customerData.city,
          });
        } catch (customerError) {
          setError('Customer not found');
          console.error('Customer fetch error:', customerError);
        }
      }
    } catch (err: any) {
      if (err.message === 'Account not found') {
        setError('Account not found');
      } else {
        setError('Failed to retrieve account information');
      }
      console.error('Account fetch error:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleConfirmUpdate = async () => {
    setError(null);
    setSuccessMessage(null);
    setFieldErrors({});

    if (!account) {
      setError('Please retrieve an account first');
      return;
    }

    const accountValid = validateAccountFields();
    const customerValid = validateCustomerFields();

    if (!accountValid || !customerValid) {
      setError('Please correct the validation errors');
      return;
    }

    try {
      setSaving(true);

      await accountService.updateAccount(account.accountId, accountFormData);

      if (customer) {
        await customerService.updateCustomer(customer.customerId, customerFormData);
      }

      setSuccessMessage('Account and customer information updated successfully');
      
      setTimeout(() => {
        setAccount(null);
        setCustomer(null);
        setAccountFormData({});
        setCustomerFormData({});
        setSearchAccountId('');
        setSuccessMessage(null);
      }, 2000);
    } catch (err: any) {
      if (err.message.includes('Concurrent update detected')) {
        setError('Concurrent update detected. The data has been refreshed with current values. Please review and try again.');
        handleRetrieveAccount();
      } else {
        setError(err.message || 'Failed to update account information');
      }
      console.error('Update error:', err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    setAccount(null);
    setCustomer(null);
    setAccountFormData({});
    setCustomerFormData({});
    setSearchAccountId('');
    setError(null);
    setSuccessMessage(null);
    setFieldErrors({});
  };

  const handleKeyPress = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') {
      handleRetrieveAccount();
    }
  };

  return (
    <div className="p-6 max-w-7xl mx-auto">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account Update</h1>
        <Button variant="secondary" onClick={() => router.push('/')}>
          Exit
        </Button>
      </div>

      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-lg font-semibold mb-4">Account Search</h2>
        <div className="flex gap-4 items-end">
          <div className="flex-1 max-w-md">
            <Input
              label="Account Number"
              value={searchAccountId}
              onChange={(e) => {
                setSearchAccountId(e.target.value);
                setError(null);
              }}
              onKeyPress={handleKeyPress}
              placeholder="Enter 11-digit account number"
              required
            />
          </div>
          <Button onClick={handleRetrieveAccount} disabled={loading}>
            {loading ? 'Loading...' : 'Retrieve Account'}
          </Button>
        </div>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-6">
          {error}
        </div>
      )}

      {successMessage && (
        <div className="bg-green-50 border border-green-200 text-green-800 px-4 py-3 rounded mb-6">
          {successMessage}
        </div>
      )}

      {account && (
        <>
          <div className="bg-white shadow rounded-lg p-6 mb-6">
            <h2 className="text-lg font-semibold mb-4">Account Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              <Input
                label="Account ID"
                value={account.accountId.toString()}
                disabled
              />
              <div>
                <Input
                  label="Active Status (Y/N)"
                  value={accountFormData.activeStatus || ''}
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
                  value={accountFormData.currentBalance?.toString() || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, currentBalance: parseFloat(e.target.value) })}
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
                  value={accountFormData.creditLimit?.toString() || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, creditLimit: parseFloat(e.target.value) })}
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
                  value={accountFormData.cashCreditLimit?.toString() || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, cashCreditLimit: parseFloat(e.target.value) })}
                  required
                  className={fieldErrors.cashCreditLimit ? 'border-red-500' : ''}
                />
                {fieldErrors.cashCreditLimit && (
                  <p className="text-red-600 text-sm mt-1">{fieldErrors.cashCreditLimit}</p>
                )}
              </div>
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
              <div>
                <Input
                  label="Current Cycle Credit"
                  type="number"
                  step="0.01"
                  value={accountFormData.currentCycleCredit?.toString() || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, currentCycleCredit: parseFloat(e.target.value) })}
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
                  value={accountFormData.currentCycleDebit?.toString() || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, currentCycleDebit: parseFloat(e.target.value) })}
                  required
                  className={fieldErrors.currentCycleDebit ? 'border-red-500' : ''}
                />
                {fieldErrors.currentCycleDebit && (
                  <p className="text-red-600 text-sm mt-1">{fieldErrors.currentCycleDebit}</p>
                )}
              </div>
              <Input
                label="Group ID"
                value={accountFormData.groupId || ''}
                onChange={(e) => setAccountFormData({ ...accountFormData, groupId: e.target.value })}
              />
            </div>
          </div>

          {customer && (
            <div className="bg-white shadow rounded-lg p-6 mb-6">
              <h2 className="text-lg font-semibold mb-4">Customer Information</h2>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                <Input
                  label="Customer ID"
                  value={customer.customerId.toString()}
                  disabled
                />
                <div>
                  <Input
                    label="First Name"
                    value={customerFormData.firstName || ''}
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
                  value={customerFormData.middleName || ''}
                  onChange={(e) => setCustomerFormData({ ...customerFormData, middleName: e.target.value })}
                />
                <div>
                  <Input
                    label="Last Name"
                    value={customerFormData.lastName || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, lastName: e.target.value })}
                    required
                    className={fieldErrors.lastName ? 'border-red-500' : ''}
                  />
                  {fieldErrors.lastName && (
                    <p className="text-red-600 text-sm mt-1">{fieldErrors.lastName}</p>
                  )}
                </div>
                <Input
                  label="Address Line 1"
                  value={customerFormData.addressLine1 || ''}
                  onChange={(e) => setCustomerFormData({ ...customerFormData, addressLine1: e.target.value })}
                  required
                />
                <Input
                  label="Address Line 2"
                  value={customerFormData.addressLine2 || ''}
                  onChange={(e) => setCustomerFormData({ ...customerFormData, addressLine2: e.target.value })}
                />
                <Input
                  label="Address Line 3"
                  value={customerFormData.addressLine3 || ''}
                  onChange={(e) => setCustomerFormData({ ...customerFormData, addressLine3: e.target.value })}
                  required
                />
                <div>
                  <Input
                    label="State Code"
                    value={customerFormData.stateCode || ''}
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
                    value={customerFormData.zipCode || ''}
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
                    value={customerFormData.countryCode || ''}
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
                    value={customerFormData.primaryPhoneNumber || ''}
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
                    value={customerFormData.secondaryPhoneNumber || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, secondaryPhoneNumber: e.target.value })}
                    placeholder="(XXX)XXX-XXXX"
                    className={fieldErrors.secondaryPhoneNumber ? 'border-red-500' : ''}
                  />
                  {fieldErrors.secondaryPhoneNumber && (
                    <p className="text-red-600 text-sm mt-1">{fieldErrors.secondaryPhoneNumber}</p>
                  )}
                </div>
                <Input
                  label="SSN"
                  type="number"
                  value={customerFormData.ssn?.toString() || ''}
                  onChange={(e) => setCustomerFormData({ ...customerFormData, ssn: parseInt(e.target.value) })}
                  required
                />
                <Input
                  label="Government Issued ID"
                  value={customerFormData.governmentIssuedId || ''}
                  onChange={(e) => setCustomerFormData({ ...customerFormData, governmentIssuedId: e.target.value })}
                />
                <Input
                  label="Date of Birth"
                  type="date"
                  value={customerFormData.dateOfBirth || ''}
                  onChange={(e) => setCustomerFormData({ ...customerFormData, dateOfBirth: e.target.value })}
                  required
                />
                <Input
                  label="EFT Account ID"
                  value={customerFormData.eftAccountId || ''}
                  onChange={(e) => setCustomerFormData({ ...customerFormData, eftAccountId: e.target.value })}
                />
                <div>
                  <Input
                    label="Primary Cardholder (Y/N)"
                    value={customerFormData.primaryCardholderIndicator || ''}
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
                    value={customerFormData.ficoScore?.toString() || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, ficoScore: parseInt(e.target.value) })}
                    required
                    className={fieldErrors.ficoScore ? 'border-red-500' : ''}
                  />
                  {fieldErrors.ficoScore && (
                    <p className="text-red-600 text-sm mt-1">{fieldErrors.ficoScore}</p>
                  )}
                </div>
              </div>
            </div>
          )}

          <div className="flex gap-4">
            <Button onClick={handleConfirmUpdate} disabled={saving}>
              {saving ? 'Saving...' : 'Confirm Update'}
            </Button>
            <Button variant="secondary" onClick={handleCancel}>
              Cancel
            </Button>
          </div>
        </>
      )}
    </div>
  );
}
