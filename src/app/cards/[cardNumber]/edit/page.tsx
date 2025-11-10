'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { Card, UpdateCardRequest } from '@/types/card';
import { Input, Select, Button } from '@/components/ui';

export default function EditCardPage() {
  const params = useParams();
  const router = useRouter();
  const [originalCard, setOriginalCard] = useState<Card | null>(null);
  const [formData, setFormData] = useState<UpdateCardRequest>({});
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [errors, setErrors] = useState<{
    embossedName?: string;
    expirationDate?: string;
    activeStatus?: string;
  }>({});

  useEffect(() => {
    if (params.cardNumber) {
      fetchCard(params.cardNumber as string);
    }
  }, [params.cardNumber]);

  const fetchCard = async (cardNumber: string) => {
    try {
      setLoading(true);
      const data = await cardService.getCardByNumber(cardNumber);
      setOriginalCard(data);
      setFormData({
        embossedName: data.embossedName,
        expirationDate: data.expirationDate.split('T')[0],
        activeStatus: data.activeStatus,
      });
    } catch (err) {
      console.error('Failed to load card:', err);
      alert('Failed to load card details');
      router.push('/cards');
    } finally {
      setLoading(false);
    }
  };

  const validateForm = (): boolean => {
    const newErrors: {
      embossedName?: string;
      expirationDate?: string;
      activeStatus?: string;
    } = {};
    let isValid = true;

    // Validate embossed name - only alphabets and spaces
    if (formData.embossedName) {
      if (!/^[A-Za-z\s]+$/.test(formData.embossedName)) {
        newErrors.embossedName = 'Embossed name can only contain alphabets and spaces';
        isValid = false;
      }
      if (formData.embossedName.trim().length === 0) {
        newErrors.embossedName = 'Embossed name is required';
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

    // Validate active status - must be Y or N
    if (formData.activeStatus) {
      if (formData.activeStatus !== 'Y' && formData.activeStatus !== 'N') {
        newErrors.activeStatus = 'Active status must be Y or N';
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
      await cardService.updateCard(params.cardNumber as string, formData);
      router.push(`/cards/${params.cardNumber}`);
    } catch (err) {
      alert('Failed to update card');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    if (confirm('Are you sure you want to cancel? Any unsaved changes will be lost.')) {
      router.push(`/cards/${params.cardNumber}`);
    }
  };

  if (loading) {
    return <div className="p-6">Loading...</div>;
  }

  if (!originalCard) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-4">
          Card not found
        </div>
        <Button variant="secondary" onClick={() => router.push('/cards')}>
          Back to Card List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-3xl">
      {/* Header Section */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Edit Card</h1>
        <p className="text-sm text-gray-600">COCRDUPC - Credit Card Update</p>
        <p className="text-sm text-gray-500 mt-1">Card Number: {originalCard.cardNumber}</p>
      </div>

      {/* Form Section */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6 space-y-6">
        {/* Read-only fields */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 pb-4 border-b">
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Card Number (Read-only)
            </label>
            <p className="text-gray-900 font-mono bg-gray-50 px-3 py-2 rounded">
              {originalCard.cardNumber}
            </p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Account ID (Read-only)
            </label>
            <p className="text-gray-900 font-mono bg-gray-50 px-3 py-2 rounded">
              {originalCard.accountId}
            </p>
          </div>
        </div>

        {/* Editable fields */}
        <div className="space-y-4">
          <Input
            label="Embossed Name"
            value={formData.embossedName || ''}
            onChange={(e) => {
              setFormData({ ...formData, embossedName: e.target.value });
              setErrors({ ...errors, embossedName: undefined });
            }}
            error={errors.embossedName}
            required
            placeholder="Enter name as it appears on card"
            helperText="Only alphabets and spaces allowed"
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
        </div>

        {/* Warning Messages */}
        {formData.activeStatus === 'N' && (
          <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ Warning</p>
            <p className="text-sm mt-1">
              Setting the card to inactive will prevent it from being used for transactions.
            </p>
          </div>
        )}

        {formData.expirationDate && new Date(formData.expirationDate) < new Date() && (
          <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ Error</p>
            <p className="text-sm mt-1">
              The expiration date must be in the future.
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
            onClick={() => router.push('/cards')}
          >
            Back to List
          </Button>
        </div>
      </form>

      {/* Information Section */}
      <div className="mt-6 bg-blue-50 border border-blue-200 text-blue-800 px-4 py-3 rounded">
        <p className="font-semibold">ℹ️ Update Information</p>
        <ul className="text-sm mt-2 list-disc list-inside space-y-1">
          <li>Card number and account ID cannot be changed</li>
          <li>Embossed name must contain only letters and spaces</li>
          <li>Expiration date must be a future date</li>
          <li>Active status must be Y (Active) or N (Inactive)</li>
        </ul>
      </div>
    </div>
  );
}
