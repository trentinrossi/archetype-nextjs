'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreateCreditCardRequest } from '@/types/creditCard';
import { Input, Select, Button } from '@/components/ui';

export default function CreateCreditCardPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateCreditCardRequest>({
    cardNumber: '',
    accountId: 0,
    embossedName: '',
    expirationDate: '',
    cardStatus: 'Y',
    cvvCode: '',
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!formData.cardNumber || !/^\d{16}$/.test(formData.cardNumber)) {
      errors.cardNumber = 'Card number must be exactly 16 digits';
    } else if (formData.cardNumber === '0000000000000000') {
      errors.cardNumber = 'Card number cannot be all zeros';
    }

    if (!formData.accountId || !/^\d{11}$/.test(formData.accountId.toString())) {
      errors.accountId = 'Account ID must be exactly 11 digits';
    } else if (formData.accountId === 0) {
      errors.accountId = 'Account ID cannot be zero';
    }

    if (!formData.embossedName || formData.embossedName.trim() === '') {
      errors.embossedName = 'Embossed name is required';
    } else if (!/^[A-Za-z\s]+$/.test(formData.embossedName)) {
      errors.embossedName = 'Embossed name must contain only letters and spaces';
    } else if (formData.embossedName.length > 50) {
      errors.embossedName = 'Embossed name must not exceed 50 characters';
    }

    if (!formData.expirationDate) {
      errors.expirationDate = 'Expiration date is required';
    } else {
      const expirationDate = new Date(formData.expirationDate);
      if (isNaN(expirationDate.getTime())) {
        errors.expirationDate = 'Invalid expiration date';
      }
    }

    if (!formData.cardStatus) {
      errors.cardStatus = 'Card status is required';
    } else if (formData.cardStatus !== 'Y' && formData.cardStatus !== 'N') {
      errors.cardStatus = 'Card status must be Y (active) or N (inactive)';
    }

    if (!formData.cvvCode || !/^\d{3}$/.test(formData.cvvCode)) {
      errors.cvvCode = 'CVV code must be exactly 3 digits';
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

      await creditCardService.createCreditCard(formData);
      router.push('/credit-cards');
    } catch (err: any) {
      setError(err.message || 'Failed to create credit card');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <h1 className="text-2xl font-bold mb-6">Create New Credit Card</h1>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-6">
          {error}
        </div>
      )}

      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6 space-y-4">
        <div>
          <Input
            label="Card Number (16 digits)"
            value={formData.cardNumber}
            onChange={(e) => {
              setFormData({ ...formData, cardNumber: e.target.value });
              setValidationErrors({ ...validationErrors, cardNumber: '' });
            }}
            placeholder="Enter 16-digit card number"
            maxLength={16}
            required
          />
          {validationErrors.cardNumber && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.cardNumber}</p>
          )}
        </div>

        <div>
          <Input
            label="Account ID (11 digits)"
            type="number"
            value={formData.accountId || ''}
            onChange={(e) => {
              setFormData({ ...formData, accountId: Number(e.target.value) });
              setValidationErrors({ ...validationErrors, accountId: '' });
            }}
            placeholder="Enter 11-digit account ID"
            required
          />
          {validationErrors.accountId && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.accountId}</p>
          )}
        </div>

        <div>
          <Input
            label="Embossed Name"
            value={formData.embossedName}
            onChange={(e) => {
              setFormData({ ...formData, embossedName: e.target.value });
              setValidationErrors({ ...validationErrors, embossedName: '' });
            }}
            placeholder="Enter name as it appears on card"
            maxLength={50}
            required
          />
          {validationErrors.embossedName && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.embossedName}</p>
          )}
          <p className="text-sm text-gray-600 mt-1">Letters and spaces only</p>
        </div>

        <div>
          <Input
            label="Expiration Date"
            type="date"
            value={formData.expirationDate}
            onChange={(e) => {
              setFormData({ ...formData, expirationDate: e.target.value });
              setValidationErrors({ ...validationErrors, expirationDate: '' });
            }}
            required
          />
          {validationErrors.expirationDate && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.expirationDate}</p>
          )}
        </div>

        <div>
          <Select
            label="Card Status"
            value={formData.cardStatus}
            onChange={(e) => {
              setFormData({ ...formData, cardStatus: e.target.value });
              setValidationErrors({ ...validationErrors, cardStatus: '' });
            }}
            options={[
              { value: 'Y', label: 'Active (Y)' },
              { value: 'N', label: 'Inactive (N)' },
            ]}
            required
          />
          {validationErrors.cardStatus && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.cardStatus}</p>
          )}
        </div>

        <div>
          <Input
            label="CVV Code (3 digits)"
            value={formData.cvvCode}
            onChange={(e) => {
              setFormData({ ...formData, cvvCode: e.target.value });
              setValidationErrors({ ...validationErrors, cvvCode: '' });
            }}
            placeholder="Enter 3-digit CVV"
            maxLength={3}
            required
          />
          {validationErrors.cvvCode && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.cvvCode}</p>
          )}
        </div>

        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Credit Card'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/credit-cards')}
          >
            Cancel
          </Button>
        </div>
      </form>

      <div className="mt-4 text-sm text-gray-600">
        <p>
          <strong>Note:</strong> All fields are required. Card number must be unique and account must exist.
        </p>
      </div>
    </div>
  );
}
