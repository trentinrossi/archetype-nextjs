'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account, AccountUpdateRequest } from '@/types/account';
import { Input, Select, Button } from '@/components/ui';

export default function EditAccountPage() {
  const params = useParams();
  const router = useRouter();
  const [originalAccount, setOriginalAccount] = useState<Account | null>(null);
  const [formData, setFormData] = useState<AccountUpdateRequest>({});
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [errors, setErrors] = useState<{
    activeStatus?: string;
    creditLimit?: string;
    cashCreditLimit?: string;
    expirationDate?: string;
    reissueDate?: string;
  }>({});

  useEffect(() => {
    if (params.accountId) {
      fetchAccount(params.accountId as string);
    }
  }, [params.accountId]);

  const fetchAccount = async (accountId: string) => {
    try {
      setLoading(true);
      const data = await accountService.getAccountById(accountId);
      setOriginalAccount(data);
      setFormData({
        activeStatus: data.activeStatus,
        creditLimit: data.creditLimit,
        cashCreditLimit: data.cashCreditLimit,
        expirationDate: data.expirationDate.split('T')[0],
        reissueDate: data.reissueDate ? data.reissueDate.split('T')[0] : undefined,
        groupId: data.groupId,
      });
    } catch (err) {
      console.error('Failed to load account:', err);
      alert('Failed to load account details');
      router.push('/accounts');
    } finally {
      setLoading(false);
    }
  };

  const validateForm = (): boolean => {
    const newErrors: {
      activeStatus?: string;
      creditLimit?: string;
      cashCreditLimit?: string;
      expirationDate?: string;
      reissueDate?: string;
    } = {};
    let isValid = true;

    // Validate active status - must be Y or N
    if (formData.activeStatus) {
      if (formData.activeStatus !== 'Y' && formData.activeStatus !== 'N') {
        newErrors.activeStatus = 'Active status must be Y or N';
        isValid = false;
      }
    }

    // Validate credit limit - must be positive
    if (formData.creditLimit !== undefined) {
      if (formData.creditLimit <= 0) {
        newErrors.creditLimit = 'Credit limit must be positive';
        isValid = false;
      }
    }

    // Validate cash credit limit - must be positive
    if (formData.cashCreditLimit !== undefined) {
      if (formData.cashCreditLimit <= 0) {
        newErrors.cashCreditLimit = 'Cash credit limit must be positive';
        isValid = false;
      }
    }

    // Validate expiration date - must be in the future
    if (formData.expirationDate) {
      const expDate = new Date(formData.expirationDate);
      const today = new Date();
      today.setHours(0, 0, 0, 0);
      
      if (expDate <= today) {
        newErrors.expirationDate = 'Expiration date must be in the future';
        isValid = false;
      }
    }

    // Validate reissue date - must be in the past or today
    if (formData.reissueDate) {
      const reissueDate = new Date(formData.reissueDate);
      const today = new Date();
      today.setHours(23, 59, 59, 999);
      
      if (reissueDate > today) {
        newErrors.reissueDate = 'Reissue date cannot be in the future';
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
      setSaving(true);
      await accountService.updateAccount(params.accountId as string, formData);
      router.push(`/accounts/${params.accountId}`);
    } catch (err) {
      alert('Failed to update account');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    if (confirm('Are you sure you want to cancel? Any unsaved changes will be lost.')) {
      router.push(`/accounts/${params.accountId}`);
    }
  };

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  if (loading) {
    return <div className="p-6">Loading...</div>;
  }

  if (!originalAccount) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-4">
          Account not found
        </div>
        <Button variant="secondary" onClick={() => router.push('/accounts')}>
          Back to Accounts
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-4xl">
      {/* Header Section */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Edit Account</h1>
        <p className="text-sm text-gray-600">COACTUPC - Account Update</p>
        <p className="text-sm text-gray-500 mt-1">Account ID: {originalAccount.accountId}</p>
      </div>

      {/* Form Section */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6 space-y-6">
        {/* Read-only fields */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 pb-4 border-b">
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Account ID (Read-only)
            </label>
            <p className="text-gray-900 font-mono bg-gray-50 px-3 py-2 rounded">
              {originalAccount.accountId}
            </p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Current Balance (Read-only)
            </label>
            <p className="text-gray-900 bg-gray-50 px-3 py-2 rounded">
              {formatCurrency(originalAccount.currentBalance)}
            </p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Available Credit (Read-only)
            </label>
            <p className="text-gray-900 bg-gray-50 px-3 py-2 rounded">
              {formatCurrency(originalAccount.availableCredit)}
            </p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Open Date (Read-only)
            </label>
            <p className="text-gray-900 bg-gray-50 px-3 py-2 rounded">
              {new Date(originalAccount.openDate).toLocaleDateString()}
            </p>
          </div>
        </div>

        {/* Editable fields */}
        <div className="space-y-4">
          <Select
            label="Active Status"
            value={formData.activeStatus || 'Y'}
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

          <Input
            label="Credit Limit"
            type="number"
            step="0.01"
            value={formData.creditLimit || 0}
            onChange={(e) => {
              setFormData({ ...formData, creditLimit: Number(e.target.value) });
              setErrors({ ...errors, creditLimit: undefined });
            }}
            error={errors.creditLimit}
            required
            helperText="Must be a positive amount"
          />

          <Input
            label="Cash Credit Limit"
            type="number"
            step="0.01"
            value={formData.cashCreditLimit || 0}
            onChange={(e) => {
              setFormData({ ...formData, cashCreditLimit: Number(e.target.value) });
              setErrors({ ...errors, cashCreditLimit: undefined });
            }}
            error={errors.cashCreditLimit}
            required
            helperText="Must be a positive amount"
          />

          <Input
            label="Expiration Date"
            type="date"
            value={formData.expirationDate || ''}
            onChange={(e) => {
              setFormData({ ...formData, expirationDate: e.target.value });
              setErrors({ ...errors, expirationDate: undefined });
            }}
            error={errors.expirationDate}
            required
            helperText="Must be a future date"
          />

          <Input
            label="Reissue Date (Optional)"
            type="date"
            value={formData.reissueDate || ''}
            onChange={(e) => {
              setFormData({ ...formData, reissueDate: e.target.value || undefined });
              setErrors({ ...errors, reissueDate: undefined });
            }}
            error={errors.reissueDate}
            helperText="Cannot be in the future"
          />

          <Input
            label="Group ID (Optional)"
            value={formData.groupId || ''}
            onChange={(e) => {
              setFormData({ ...formData, groupId: e.target.value || undefined });
            }}
            placeholder="Enter group ID if applicable"
          />
        </div>

        {/* Warning Messages */}
        {formData.activeStatus === 'N' && (
          <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ Warning</p>
            <p className="text-sm mt-1">
              Setting the account to inactive will prevent all associated cards from being used.
            </p>
          </div>
        )}

        {formData.creditLimit !== undefined && 
         originalAccount.currentBalance > formData.creditLimit && (
          <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ Error</p>
            <p className="text-sm mt-1">
              Credit limit cannot be less than current balance ({formatCurrency(originalAccount.currentBalance)}).
            </p>
          </div>
        )}

        {/* Action Buttons */}
        <div className="flex gap-2 pt-4 border-t">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Save Changes'}
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
        <p className="font-semibold">ℹ️ Update Information</p>
        <ul className="text-sm mt-2 list-disc list-inside space-y-1">
          <li>Account ID, balance, and open date cannot be changed</li>
          <li>Credit limits must be positive amounts</li>
          <li>Credit limit cannot be less than current balance</li>
          <li>Expiration date must be in the future</li>
          <li>Reissue date cannot be in the future</li>
          <li>Active status must be Y (Active) or N (Inactive)</li>
        </ul>
      </div>
    </div>
  );
}
