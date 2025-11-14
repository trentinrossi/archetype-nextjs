'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { customerService } from '@/services/customerService';
import { Account } from '@/types/account';
import { Customer } from '@/types/customer';
import { Input, Button } from '@/components/ui';

export default function AccountViewPage() {
  const router = useRouter();
  const [accountId, setAccountId] = useState('');
  const [account, setAccount] = useState<Account | null>(null);
  const [customer, setCustomer] = useState<Customer | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [infoMessage, setInfoMessage] = useState<string | null>(null);
  const [accountIdError, setAccountIdError] = useState(false);

  const validateAccountId = (value: string): boolean => {
    if (!value || value.trim() === '') {
      setAccountIdError(true);
      setError('Account number is required');
      return false;
    }
    
    if (!/^\d{11}$/.test(value)) {
      setAccountIdError(true);
      setError('Account number must be 11 digits');
      return false;
    }
    
    if (value === '00000000000') {
      setAccountIdError(true);
      setError('Account number must be non-zero');
      return false;
    }
    
    setAccountIdError(false);
    return true;
  };

  const handleViewAccount = async () => {
    setError(null);
    setInfoMessage(null);
    setAccount(null);
    setCustomer(null);

    if (!validateAccountId(accountId)) {
      return;
    }

    try {
      setLoading(true);
      
      const accountData = await accountService.getAccountById(accountId);
      setAccount(accountData);
      
      if (accountData.customerId) {
        try {
          const customerData = await customerService.getCustomerById(accountData.customerId);
          setCustomer(customerData);
          setInfoMessage('Account and customer information retrieved successfully');
        } catch (customerError) {
          setError('Customer not found');
          console.error('Customer fetch error:', customerError);
        }
      }
    } catch (err: any) {
      if (err.message === 'Account not found') {
        setError('Account not found in any file');
      } else {
        setError('Failed to retrieve account information');
      }
      console.error('Account fetch error:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleKeyPress = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') {
      handleViewAccount();
    }
  };

  const formatCurrency = (value: number | undefined): string => {
    if (value === undefined || value === null) return '$0.00';
    return `$${value.toFixed(2)}`;
  };

  const formatDate = (dateString: string | undefined): string => {
    if (!dateString) return '';
    return new Date(dateString).toLocaleDateString();
  };

  const formatSSN = (ssn: string | undefined): string => {
    if (!ssn) return '';
    if (ssn.length === 9) {
      return `${ssn.substring(0, 3)}-${ssn.substring(3, 5)}-${ssn.substring(5)}`;
    }
    return ssn;
  };

  return (
    <div className="p-6 max-w-7xl mx-auto">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account View</h1>
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
              value={accountId}
              onChange={(e) => {
                setAccountId(e.target.value);
                setAccountIdError(false);
                setError(null);
              }}
              onKeyPress={handleKeyPress}
              placeholder="Enter 11-digit account number"
              required
              className={accountIdError ? 'border-red-500' : ''}
            />
          </div>
          <Button onClick={handleViewAccount} disabled={loading}>
            {loading ? 'Loading...' : 'View Account'}
          </Button>
        </div>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-6">
          {error}
        </div>
      )}

      {infoMessage && (
        <div className="bg-blue-50 border border-blue-200 text-blue-800 px-4 py-3 rounded mb-6">
          {infoMessage}
        </div>
      )}

      {account && (
        <>
          <div className="bg-white shadow rounded-lg p-6 mb-6">
            <h2 className="text-lg font-semibold mb-4">Account Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              <div>
                <label className="block text-sm font-semibold text-gray-700">Account Status</label>
                <p className="mt-1 text-gray-900">{account.accountStatus || 'N/A'}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Current Balance</label>
                <p className="mt-1 text-gray-900">{formatCurrency(account.currentBalance)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Credit Limit</label>
                <p className="mt-1 text-gray-900">{formatCurrency(account.creditLimit)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Cash Credit Limit</label>
                <p className="mt-1 text-gray-900">{formatCurrency(account.cashCreditLimit)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Available Credit</label>
                <p className="mt-1 text-gray-900">{formatCurrency(account.availableCredit)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Current Cycle Credit</label>
                <p className="mt-1 text-gray-900">{formatCurrency(account.currentCycleCredit)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Current Cycle Debit</label>
                <p className="mt-1 text-gray-900">{formatCurrency(account.currentCycleDebit)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Open Date</label>
                <p className="mt-1 text-gray-900">{formatDate(account.openDate)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Expiration Date</label>
                <p className="mt-1 text-gray-900">{formatDate(account.expirationDate)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Reissue Date</label>
                <p className="mt-1 text-gray-900">{formatDate(account.reissueDate)}</p>
              </div>
              <div>
                <label className="block text-sm font-semibold text-gray-700">Group ID</label>
                <p className="mt-1 text-gray-900">{account.groupId || 'N/A'}</p>
              </div>
            </div>
          </div>

          {customer && (
            <div className="bg-white shadow rounded-lg p-6">
              <h2 className="text-lg font-semibold mb-4">Customer Information</h2>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Customer ID</label>
                  <p className="mt-1 text-gray-900">{customer.customerId}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">SSN</label>
                  <p className="mt-1 text-gray-900">{formatSSN(customer.ssn)}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">FICO Credit Score</label>
                  <p className="mt-1 text-gray-900">{customer.ficoCreditScore || 'N/A'}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Date of Birth</label>
                  <p className="mt-1 text-gray-900">{formatDate(customer.dateOfBirth)}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">First Name</label>
                  <p className="mt-1 text-gray-900">{customer.firstName}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Middle Name</label>
                  <p className="mt-1 text-gray-900">{customer.middleName || 'N/A'}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Last Name</label>
                  <p className="mt-1 text-gray-900">{customer.lastName}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Address Line 1</label>
                  <p className="mt-1 text-gray-900">{customer.addressLine1}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Address Line 2</label>
                  <p className="mt-1 text-gray-900">{customer.addressLine2 || 'N/A'}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">City</label>
                  <p className="mt-1 text-gray-900">{customer.city}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">State Code</label>
                  <p className="mt-1 text-gray-900">{customer.stateCode}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">ZIP Code</label>
                  <p className="mt-1 text-gray-900">{customer.zipCode}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Country Code</label>
                  <p className="mt-1 text-gray-900">{customer.countryCode}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Primary Phone</label>
                  <p className="mt-1 text-gray-900">{customer.primaryPhoneNumber || customer.phoneNumber1}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Secondary Phone</label>
                  <p className="mt-1 text-gray-900">{customer.secondaryPhoneNumber || customer.phoneNumber2 || 'N/A'}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Government ID</label>
                  <p className="mt-1 text-gray-900">{customer.governmentIssuedId || 'N/A'}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">EFT Account ID</label>
                  <p className="mt-1 text-gray-900">{customer.eftAccountId || 'N/A'}</p>
                </div>
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Primary Cardholder</label>
                  <p className="mt-1 text-gray-900">{customer.primaryCardholderIndicator === 'Y' ? 'Yes' : 'No'}</p>
                </div>
              </div>
            </div>
          )}
        </>
      )}
    </div>
  );
}
