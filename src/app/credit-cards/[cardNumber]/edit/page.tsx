'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreditCard, UpdateCreditCardRequest } from '@/types/creditCard';
import { Input, Select, Button } from '@/components/ui';

export default function EditCreditCardPage() {
  const params = useParams();
  const router = useRouter();
  const [originalCard, setOriginalCard] = useState<CreditCard | null>(null);
  const [formData, setFormData] = useState<UpdateCreditCardRequest>({
    embossedName: '',
    cardStatus: 'Y',
    expirationMonth: 1,
    expirationYear: new Date().getFullYear(),
    expirationDay: 31,
    version: 0,
  });
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});
  const [confirmationPending, setConfirmationPending] = useState(false);

  useEffect(() => {
    if (params.cardNumber) {
      fetchCreditCard(params.cardNumber as string);
    }
  }, [params.cardNumber]);

  const fetchCreditCard = async (cardNumber: string) => {
    try {
      setLoading(true);
      const data = await creditCardService.getCreditCardByCardNumber(cardNumber);
      setOriginalCard(data);
      setFormData({
        embossedName: data.embossedName,
        cardStatus: data.cardStatus,
        expirationMonth: data.expirationMonth,
        expirationYear: data.expirationYear,
        expirationDay: data.expirationDay,
        version: data.version,
      });
      setError(null);
    } catch (err: any) {
      setError(err.message || 'Failed to load credit card');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!formData.embossedName || formData.embossedName.trim() === '') {
      errors.embossedName = 'Embossed name is required';
    } else if (!/^[A-Za-z\s]+$/.test(formData.embossedName)) {
      errors.embossedName = 'Embossed name must contain only letters and spaces';
    } else if (formData.embossedName.length > 50) {
      errors.embossedName = 'Embossed name must not exceed 50 characters';
    }

    if (!formData.cardStatus) {
      errors.cardStatus = 'Card status is required';
    } else if (formData.cardStatus !== 'Y' && formData.cardStatus !== 'N') {
      errors.cardStatus = 'Card status must be Y (active) or N (inactive)';
    }

    if (!formData.expirationMonth || formData.expirationMonth < 1 || formData.expirationMonth > 12) {
      errors.expirationMonth = 'Expiration month must be between 1 and 12';
    }

    if (!formData.expirationYear || formData.expirationYear < 1950 || formData.expirationYear > 2099) {
      errors.expirationYear = 'Expiration year must be between 1950 and 2099';
    }

    if (formData.expirationDay && (formData.expirationDay < 1 || formData.expirationDay > 31)) {
      errors.expirationDay = 'Expiration day must be between 1 and 31';
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

    setConfirmationPending(true);
    setError(null);
  };

  const handleConfirmUpdate = async () => {
    if (!originalCard) return;

    try {
      setSaving(true);
      setError(null);

      const userId = localStorage.getItem('user_id');
      const updateData: UpdateCreditCardRequest = {
        ...formData,
        updatedBy: userId || undefined,
      };

      await creditCardService.updateCreditCard(originalCard.cardNumber, updateData);
      router.push(`/credit-cards/${originalCard.cardNumber}`);
    } catch (err: any) {
      setError(err.message || 'Failed to update credit card');
      setConfirmationPending(false);
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancelConfirmation = () => {
    setConfirmationPending(false);
  };

  if (loading) {
    return <div className="p-6">Loading credit card details...</div>;
  }

  if (error && !originalCard) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
        <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
          Back to List
        </Button>
      </div>
    );
  }

  if (!originalCard) {
    return (
      <div className="p-6">
        <div className="text-gray-600 mb-4">Credit card not found</div>
        <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
          Back to List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Update Credit Card</h1>
        <Button
          variant="secondary"
          onClick={() => router.push(`/credit-cards/${originalCard.cardNumber}`)}
        >
          Cancel
        </Button>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-6">
          {error}
        </div>
      )}

      {confirmationPending && (
        <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded mb-6">
          <p className="font-semibold mb-2">Confirm Update</p>
          <p className="mb-4">
            Are you sure you want to update this credit card? Please review your changes and confirm.
          </p>
          <div className="flex gap-2">
            <Button onClick={handleConfirmUpdate} disabled={saving}>
              {saving ? 'Updating...' : 'Confirm Update'}
            </Button>
            <Button variant="secondary" onClick={handleCancelConfirmation} disabled={saving}>
              Cancel
            </Button>
          </div>
        </div>
      )}

      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-lg font-semibold mb-4">Card Information (Read-Only)</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Card Number
            </label>
            <p className="text-gray-900">{originalCard.maskedCardNumber}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Account Number
            </label>
            <p className="text-gray-900">{originalCard.accountId}</p>
          </div>
        </div>
      </div>

      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6 space-y-4">
        <h2 className="text-lg font-semibold mb-4">Editable Card Details</h2>

        <div>
          <Input
            label="Embossed Name"
            value={formData.embossedName || ''}
            onChange={(e) => {
              setFormData({ ...formData, embossedName: e.target.value });
              setValidationErrors({ ...validationErrors, embossedName: '' });
            }}
            required
            maxLength={50}
            disabled={confirmationPending}
          />
          {validationErrors.embossedName && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.embossedName}</p>
          )}
        </div>

        <div>
          <Select
            label="Card Status"
            value={formData.cardStatus || 'Y'}
            onChange={(e) => {
              setFormData({ ...formData, cardStatus: e.target.value });
              setValidationErrors({ ...validationErrors, cardStatus: '' });
            }}
            options={[
              { value: 'Y', label: 'Active (Y)' },
              { value: 'N', label: 'Inactive (N)' },
            ]}
            required
            disabled={confirmationPending}
          />
          {validationErrors.cardStatus && (
            <p className="text-red-600 text-sm mt-1">{validationErrors.cardStatus}</p>
          )}
        </div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div>
            <Input
              label="Expiration Month (1-12)"
              type="number"
              value={formData.expirationMonth || ''}
              onChange={(e) => {
                setFormData({ ...formData, expirationMonth: Number(e.target.value) });
                setValidationErrors({ ...validationErrors, expirationMonth: '' });
              }}
              min={1}
              max={12}
              required
              disabled={confirmationPending}
            />
            {validationErrors.expirationMonth && (
              <p className="text-red-600 text-sm mt-1">{validationErrors.expirationMonth}</p>
            )}
          </div>

          <div>
            <Input
              label="Expiration Year (1950-2099)"
              type="number"
              value={formData.expirationYear || ''}
              onChange={(e) => {
                setFormData({ ...formData, expirationYear: Number(e.target.value) });
                setValidationErrors({ ...validationErrors, expirationYear: '' });
              }}
              min={1950}
              max={2099}
              required
              disabled={confirmationPending}
            />
            {validationErrors.expirationYear && (
              <p className="text-red-600 text-sm mt-1">{validationErrors.expirationYear}</p>
            )}
          </div>

          <div>
            <Input
              label="Expiration Day (1-31)"
              type="number"
              value={formData.expirationDay || ''}
              onChange={(e) => {
                setFormData({ ...formData, expirationDay: Number(e.target.value) });
                setValidationErrors({ ...validationErrors, expirationDay: '' });
              }}
              min={1}
              max={31}
              disabled={confirmationPending}
            />
            {validationErrors.expirationDay && (
              <p className="text-red-600 text-sm mt-1">{validationErrors.expirationDay}</p>
            )}
          </div>
        </div>

        {!confirmationPending && (
          <div className="flex gap-2 pt-4">
            <Button type="submit" disabled={saving}>
              Update Card
            </Button>
            <Button
              type="button"
              variant="secondary"
              onClick={() => router.push(`/credit-cards/${originalCard.cardNumber}`)}
            >
              Cancel
            </Button>
          </div>
        )}
      </form>

      <div className="mt-4 text-sm text-gray-600">
        <p>
          <strong>Note:</strong> Only embossed name, card status, and expiration date can be modified.
          Card number and account number cannot be changed.
        </p>
        <p className="mt-2">
          <strong>Version:</strong> {formData.version} (used for concurrent update prevention)
        </p>
      </div>
    </div>
  );
}
