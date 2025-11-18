'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { CreateAccountRequest } from '@/types/account';
import { Input, Button } from '@/components/ui';

export default function CreateAccountPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateAccountRequest>({
    accountId: '',
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!formData.accountId || !/^\d{11}$/.test(formData.accountId)) {
      errors.accountId = 'Account ID must be exactly 11 digits';
    } else if (formData.accountId === '00000000000') {
      errors.accountId = 'Account ID cannot be all zeros';
    } else if (formData.accountId === '0') {
      errors.accountId = 'Account ID cannot be zero';
    }

    setValidationErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!validateForm()) {
      setError('Please correct the validation errors before submitting');
      return;
    }

    try {
      setLoading(true);
      setError(null);

      await accountService.createAccount(formData);
      router.push('/accounts');
    } catch (err: any) {
      setError(err.message || 'Failed to create account');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-2xl mx-auto">
      <h1 className="text-2xl font-bold mb-6">Create New Account</h1>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-6">
          {error}
        </div>
      )}

      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6 space-y-4">
        <div>
          <Input
            label="Account ID (11 digits)"
            value={formData.accountId}
            onChange={(e) => {
              setFormData({ ...formData, accountId: e.target.value });
              setValidationErrors({ ...validationErrors, accountId: '' });
            }}
            placeholder="Enter 11-digit account ID"
            maxLength={11}
            required
          />
          {validationErrors.accountId && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.accountId}</p>
          )}
          <p className="text-sm text-gray-600 mt-1">
            Must be exactly 11 numeric digits, cannot be zero or all zeros
          </p>
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

      <div className="mt-4 text-sm text-gray-600">
        <p>
          <strong>Note:</strong> Account ID must be unique. After creating the account, you can add credit cards to it.
        </p>
      </div>
    </div>
  );
}
