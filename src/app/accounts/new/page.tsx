'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { AccountCreateRequest } from '@/types/account';
import { Input, Select, Button } from '@/components/ui';

export default function CreateAccountPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<AccountCreateRequest>({
    accountId: '',
    activeStatus: 'Y',
    creditLimit: 5000,
    cashCreditLimit: 1000,
    openDate: new Date().toISOString().split('T')[0],
    expirationDate: '',
    groupId: '',
  });
  const [loading, setLoading] = useState(false);
  const [errors, setErrors] = useState<{
    accountId?: string;
    activeStatus?: string;
    creditLimit?: string;
    cashCreditLimit?: string;
    openDate?: string;
    expirationDate?: string;
  }>({});

  const validateForm = (): boolean => {
    const newErrors: {
      accountId?: string;
      activeStatus?: string;
      creditLimit?: string;
      cashCreditLimit?: string;
      openDate?: string;
      expirationDate?: string;
    } = {};
    let isValid = true;

    // Validate account ID - must be 11 digits
    if (!formData.accountId || formData.accountId.trim() === '') {
      newErrors.accountId = 'Account ID is required';
      isValid = false;
    } else if (!/^\d{11}$/.test(formData.accountId.trim())) {
      newErrors.accountId = 'Account ID must be exactly 11 digits';
      isValid = false;
    }

    // Validate active status - must be Y or N
    if (!formData.activeStatus) {
      newErrors.activeStatus = 'Active status is required';
      isValid = false;
    } else if (formData.activeStatus !== 'Y' && formData.activeStatus !== 'N') {
      newErrors.activeStatus = 'Active status must be Y or N';
      isValid = false;
    }

    // Validate credit limit - must be positive
    if (formData.creditLimit <= 0) {
      newErrors.creditLimit = 'Credit limit must be positive';
      isValid = false;
    }

    // Validate cash credit limit - must be positive
    if (formData.cashCreditLimit <= 0) {
      newErrors.cashCreditLimit = 'Cash credit limit must be positive';
      isValid = false;
    }

    // Validate open date - cannot be in the future
    if (!formData.openDate) {
      newErrors.openDate = 'Open date is required';
      isValid = false;
    } else {
      const openDate = new Date(formData.openDate);
      const today = new Date();
      today.setHours(23, 59, 59, 999);
      
      if (openDate > today) {
        newErrors.openDate = 'Open date cannot be in the future';
        isValid = false;
      }
    }

    // Validate expiration date - must be in the future
    if (!formData.expirationDate) {
      newErrors.expirationDate = 'Expiration date is required';
      isValid = false;
    } else {
      const expDate = new Date(formData.expirationDate);
      const today = new Date();
      today.setHours(0, 0, 0, 0);
      
      if (expDate <= today) {
        newErrors.expirationDate = 'Expiration date must be in the future';
        isValid = false;
      }
    }

    setErrors(newErrors);
    return isValid;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!validateForm()) {
      return;
    }

    try {
      setLoading(true);
      const cleanedData: AccountCreateRequest = {
        accountId: formData.accountId.trim(),
        activeStatus: formData.activeStatus,
        creditLimit: formData.creditLimit,
        cashCreditLimit: formData.cashCreditLimit,
        openDate: formData.openDate,
        expirationDate: formData.expirationDate,
        groupId: formData.groupId?.trim() || undefined,
      };
      
      const newAccount = await accountService.createAccount(cleanedData);
      router.push(`/accounts/${newAccount.accountId}`);
    } catch (err) {
      alert('Failed to create account. Please check if the account ID is unique.');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleCancel = () => {
    if (confirm('Are you sure you want to cancel? All entered data will be lost.')) {
      router.push('/accounts');
    }
  };

  const generateAccountId = () => {
    const randomNumber = Math.floor(Math.random() * 100000000000).toString().padStart(11, '0');
    setFormData({ ...formData, accountId: randomNumber });
    setErrors({ ...errors, accountId: undefined });
  };

  const getDefaultExpirationDate = () => {
    const date = new Date();
    date.setFullYear(date.getFullYear() + 5);
    return date.toISOString().split('T')[0];
  };

  return (
    <div className="p-6 max-w-4xl">
      {/* Header Section */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Create New Account</h1>
        <p className="text-sm text-gray-600">Add a new account to the system</p>
      </div>

      {/* Form Section */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6 space-y-6">
        <div className="space-y-4">
          {/* Account ID */}
          <div>
            <div className="flex items-end gap-2">
              <div className="flex-1">
                <Input
                  label="Account ID"
                  value={formData.accountId}
                  onChange={(e) => {
                    setFormData({ ...formData, accountId: e.target.value });
                    setErrors({ ...errors, accountId: undefined });
                  }}
                  error={errors.accountId}
                  required
                  placeholder="Enter 11-digit account ID"
                  helperText="Must be exactly 11 digits and unique"
                  maxLength={11}
                />
              </div>
              <Button
                type="button"
                variant="secondary"
                onClick={generateAccountId}
              >
                Generate
              </Button>
            </div>
          </div>

          {/* Active Status */}
          <Select
            label="Active Status"
            value={formData.activeStatus}
            onChange={(e) => {
              setFormData({ ...formData, activeStatus: e.target.value });
              setErrors({ ...errors, activeStatus: undefined });
            }}
            options={[
              { value: 'Y', label: 'Active (Y)' },
              { value: 'N', label: 'Inactive (N)' },
            ]}
            error={errors.activeStatus}
            required
          />

          {/* Credit Limit */}
          <Input
            label="Credit Limit"
            type="number"
            step="0.01"
            value={formData.creditLimit}
            onChange={(e) => {
              setFormData({ ...formData, creditLimit: Number(e.target.value) });
              setErrors({ ...errors, creditLimit: undefined });
            }}
            error={errors.creditLimit}
            required
            helperText="Must be a positive amount"
          />

          {/* Cash Credit Limit */}
          <Input
            label="Cash Credit Limit"
            type="number"
            step="0.01"
            value={formData.cashCreditLimit}
            onChange={(e) => {
              setFormData({ ...formData, cashCreditLimit: Number(e.target.value) });
              setErrors({ ...errors, cashCreditLimit: undefined });
            }}
            error={errors.cashCreditLimit}
            required
            helperText="Must be a positive amount"
          />

          {/* Open Date */}
          <Input
            label="Open Date"
            type="date"
            value={formData.openDate}
            onChange={(e) => {
              setFormData({ ...formData, openDate: e.target.value });
              setErrors({ ...errors, openDate: undefined });
            }}
            error={errors.openDate}
            required
            helperText="Cannot be in the future"
          />

          {/* Expiration Date */}
          <div>
            <div className="flex items-end gap-2">
              <div className="flex-1">
                <Input
                  label="Expiration Date"
                  type="date"
                  value={formData.expirationDate}
                  onChange={(e) => {
                    setFormData({ ...formData, expirationDate: e.target.value });
                    setErrors({ ...errors, expirationDate: undefined });
                  }}
                  error={errors.expirationDate}
                  required
                  helperText="Must be a future date"
                />
              </div>
              <Button
                type="button"
                variant="secondary"
                onClick={() => {
                  setFormData({ ...formData, expirationDate: getDefaultExpirationDate() });
                  setErrors({ ...errors, expirationDate: undefined });
                }}
              >
                +5 Years
              </Button>
            </div>
          </div>

          {/* Group ID */}
          <Input
            label="Group ID (Optional)"
            value={formData.groupId || ''}
            onChange={(e) => {
              setFormData({ ...formData, groupId: e.target.value });
            }}
            placeholder="Enter group ID if applicable"
          />
        </div>

        {/* Warning Messages */}
        {formData.activeStatus === 'N' && (
          <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ Warning</p>
            <p className="text-sm mt-1">
              You are creating an inactive account. It will not be usable until activated.
            </p>
          </div>
        )}

        {formData.cashCreditLimit > formData.creditLimit && (
          <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ Warning</p>
            <p className="text-sm mt-1">
              Cash credit limit is higher than the overall credit limit. This is unusual.
            </p>
          </div>
        )}

        {/* Action Buttons */}
        <div className="flex gap-2 pt-4 border-t">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Account'}
          </Button>
          <Button type="button" variant="secondary" onClick={handleCancel}>
            Cancel
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/accounts')}
          >
            Back to List
          </Button>
        </div>
      </form>

      {/* Information Section */}
      <div className="mt-6 bg-blue-50 border border-blue-200 text-blue-800 px-4 py-3 rounded">
        <p className="font-semibold">ℹ️ Account Creation Guidelines</p>
        <ul className="text-sm mt-2 list-disc list-inside space-y-1">
          <li>Account ID must be unique and exactly 11 digits</li>
          <li>Credit limit and cash credit limit must be positive amounts</li>
          <li>Open date cannot be in the future</li>
          <li>Expiration date must be in the future (typically 5 years from open date)</li>
          <li>Active status determines if the account can be used immediately</li>
          <li>Group ID is optional and used for account grouping</li>
          <li>Use the "Generate" button to create a random account ID</li>
          <li>Use the "+5 Years" button to set expiration date 5 years from today</li>
        </ul>
      </div>
    </div>
  );
}
