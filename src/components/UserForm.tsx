'use client';

import React, { useState, useEffect } from 'react';
import { Input, Select, Button } from '@/components/ui';
import { CreateUserRequest, UpdateUserRequest } from '@/types/user';

interface UserFormProps {
  onSubmit: (data: CreateUserRequest | UpdateUserRequest) => void | Promise<void>;
  onCancel: () => void;
  initialData?: Partial<CreateUserRequest>;
}

interface FormErrors {
  userId?: string;
  firstName?: string;
  lastName?: string;
  password?: string;
  userType?: string;
}

export default function UserForm({ onSubmit, onCancel, initialData }: UserFormProps) {
  const isEditMode = !!initialData?.userId;

  const [formData, setFormData] = useState<CreateUserRequest>({
    userId: initialData?.userId || '',
    firstName: initialData?.firstName || '',
    lastName: initialData?.lastName || '',
    password: initialData?.password || '',
    userType: initialData?.userType || 'R',
  });

  const [errors, setErrors] = useState<FormErrors>({});
  const [isSubmitting, setIsSubmitting] = useState(false);

  useEffect(() => {
    if (initialData) {
      setFormData({
        userId: initialData.userId || '',
        firstName: initialData.firstName || '',
        lastName: initialData.lastName || '',
        password: initialData.password || '',
        userType: initialData.userType || 'R',
      });
    }
  }, [initialData]);

  const validateField = (name: keyof CreateUserRequest, value: string): string | undefined => {
    if (!value || value.trim() === '') {
      return 'This field is required';
    }

    switch (name) {
      case 'userId':
        if (value.length > 8) {
          return 'User ID cannot exceed 8 characters';
        }
        break;
      case 'firstName':
        if (value.length > 20) {
          return 'First name cannot exceed 20 characters';
        }
        break;
      case 'lastName':
        if (value.length > 20) {
          return 'Last name cannot exceed 20 characters';
        }
        break;
      case 'password':
        if (value.length > 8) {
          return 'Password cannot exceed 8 characters';
        }
        break;
      case 'userType':
        if (value !== 'A' && value !== 'R') {
          return 'User type must be Admin or Regular';
        }
        break;
    }

    return undefined;
  };

  const validateForm = (): boolean => {
    const newErrors: FormErrors = {};

    const userIdError = validateField('userId', formData.userId);
    if (userIdError) newErrors.userId = userIdError;

    const firstNameError = validateField('firstName', formData.firstName);
    if (firstNameError) newErrors.firstName = firstNameError;

    const lastNameError = validateField('lastName', formData.lastName);
    if (lastNameError) newErrors.lastName = lastNameError;

    const passwordError = validateField('password', formData.password);
    if (passwordError) newErrors.password = passwordError;

    const userTypeError = validateField('userType', formData.userType);
    if (userTypeError) newErrors.userType = userTypeError;

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleChange = (field: keyof CreateUserRequest, value: string) => {
    setFormData((prev) => ({
      ...prev,
      [field]: value,
    }));

    const fieldError = validateField(field, value);
    setErrors((prev) => ({
      ...prev,
      [field]: fieldError,
    }));
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!validateForm()) {
      return;
    }

    try {
      setIsSubmitting(true);

      if (isEditMode) {
        const updateData: UpdateUserRequest = {
          firstName: formData.firstName,
          lastName: formData.lastName,
          password: formData.password,
          userType: formData.userType,
        };
        await onSubmit(updateData);
      } else {
        await onSubmit(formData);
      }
    } catch (error) {
      console.error('Form submission error:', error);
    } finally {
      setIsSubmitting(false);
    }
  };

  const handleCancel = () => {
    setFormData({
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      userType: 'R',
    });
    setErrors({});
    onCancel();
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-4">
      <div>
        <Input
          label="User ID"
          value={formData.userId}
          onChange={(e) => handleChange('userId', e.target.value)}
          maxLength={8}
          required
          disabled={isEditMode}
          placeholder="Max 8 characters"
        />
        {errors.userId && (
          <p className="mt-1 text-sm text-red-600">{errors.userId}</p>
        )}
      </div>

      <div>
        <Input
          label="First Name"
          value={formData.firstName}
          onChange={(e) => handleChange('firstName', e.target.value)}
          maxLength={20}
          required
          placeholder="Max 20 characters"
        />
        {errors.firstName && (
          <p className="mt-1 text-sm text-red-600">{errors.firstName}</p>
        )}
      </div>

      <div>
        <Input
          label="Last Name"
          value={formData.lastName}
          onChange={(e) => handleChange('lastName', e.target.value)}
          maxLength={20}
          required
          placeholder="Max 20 characters"
        />
        {errors.lastName && (
          <p className="mt-1 text-sm text-red-600">{errors.lastName}</p>
        )}
      </div>

      <div>
        <Input
          label="Password"
          type="password"
          value={formData.password}
          onChange={(e) => handleChange('password', e.target.value)}
          maxLength={8}
          required
          placeholder="Max 8 characters"
        />
        {errors.password && (
          <p className="mt-1 text-sm text-red-600">{errors.password}</p>
        )}
      </div>

      <div>
        <Select
          label="User Type"
          value={formData.userType}
          onChange={(e) => handleChange('userType', e.target.value)}
          options={[
            { value: 'R', label: 'Regular' },
            { value: 'A', label: 'Admin' },
          ]}
          required
        />
        {errors.userType && (
          <p className="mt-1 text-sm text-red-600">{errors.userType}</p>
        )}
      </div>

      <div className="flex gap-2 pt-4">
        <Button type="submit" disabled={isSubmitting}>
          {isSubmitting ? 'Submitting...' : isEditMode ? 'Update User' : 'Create User'}
        </Button>
        <Button
          type="button"
          variant="secondary"
          onClick={handleCancel}
          disabled={isSubmitting}
        >
          Cancel
        </Button>
      </div>
    </form>
  );
}
