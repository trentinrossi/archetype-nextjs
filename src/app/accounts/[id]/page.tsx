'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { customerService } from '@/services/customerService';
import { Account } from '@/types/account';
import { Customer } from '@/types/customer';
import { Button } from '@/components/ui';

export default function AccountDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [customer, setCustomer] = useState<Customer | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.id) {
      fetchAccount(params.id as string);
    }
  }, [params.id]);

  const fetchAccount = async (id: string) => {
    try {
      const accountData = await accountService.getAccountById(id);
      setAccount(accountData);
      
      if (accountData.customerId) {
        try {
          const customerData = await customerService.getCustomerById(accountData.customerId);
          setCustomer(customerData);
        } catch (customerError) {
          console.error('Failed to load customer:', customerError);
        }
      }
    } catch (err) {
      setError('Failed to load account');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this account?')) return;
    
    try {
      await accountService.deleteAccount(params.id as string);
      router.push('/accounts');
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
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

  if (loading) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;
  if (!account) return <div className="p-6">Account not found</div>;

  return (
    <div className="p-6 max-w-7xl mx-auto">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account Details</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/accounts/${account.accountId}/edit`)}>
            Edit
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/accounts')}>
            Back to List
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-lg font-semibold mb-4">Account Information</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          <div>
            <label className="block text-sm font-semibold text-gray-700">Account ID</label>
            <p className="mt-1 text-gray-900">{account.accountId}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Active Status</label>
            <p className="mt-1">
              <span className={`px-2 py-1 rounded text-xs font-semibold ${
                account.activeStatus === 'Y' ? 'bg-green-100 text-green-800' : 'bg-gray-100 text-gray-800'
              }`}>
                {account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
              </span>
            </p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Account Status</label>
            <p className="mt-1 text-gray-900">{account.accountStatus}</p>
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
            <label className="block text-sm font-semibold text-gray-700">Available Credit</label>
            <p className="mt-1 text-gray-900">{formatCurrency(account.availableCredit)}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Cash Credit Limit</label>
            <p className="mt-1 text-gray-900">{formatCurrency(account.cashCreditLimit)}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Available Cash Credit</label>
            <p className="mt-1 text-gray-900">{formatCurrency(account.availableCashCredit)}</p>
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
            <label className="block text-sm font-semibold text-gray-700">Net Cycle Amount</label>
            <p className="mt-1 text-gray-900">{formatCurrency(account.netCycleAmount)}</p>
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
          <div>
            <label className="block text-sm font-semibold text-gray-700">Customer ID</label>
            <p className="mt-1 text-gray-900">{account.customerId}</p>
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
              <label className="block text-sm font-semibold text-gray-700">Full Name</label>
              <p className="mt-1 text-gray-900">{customer.fullName || `${customer.firstName} ${customer.middleName || ''} ${customer.lastName}`.trim()}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">SSN</label>
              <p className="mt-1 text-gray-900">{formatSSN(customer.ssn)}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">Date of Birth</label>
              <p className="mt-1 text-gray-900">{formatDate(customer.dateOfBirth)}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">Age</label>
              <p className="mt-1 text-gray-900">{customer.age || 'N/A'}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">FICO Score</label>
              <p className="mt-1 text-gray-900">{customer.ficoScore} {customer.ficoScoreRating && `(${customer.ficoScoreRating})`}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">Primary Cardholder</label>
              <p className="mt-1 text-gray-900">{customer.primaryCardholderStatus || (customer.primaryCardholderIndicator === 'Y' ? 'Yes' : 'No')}</p>
            </div>
            <div className="md:col-span-2 lg:col-span-3">
              <label className="block text-sm font-semibold text-gray-700">Address</label>
              <p className="mt-1 text-gray-900">{customer.fullAddress || `${customer.addressLine1}, ${customer.addressLine2 ? customer.addressLine2 + ', ' : ''}${customer.city}, ${customer.stateCode} ${customer.zipCode}, ${customer.countryCode}`}</p>
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
          </div>
        </div>
      )}
    </div>
  );
}
